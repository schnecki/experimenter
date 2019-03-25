{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Experimenter.Result.Query
    ( loadExperiment
    ) where


import           Control.Lens                (view)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.ByteString             (ByteString)
import           Data.Function               (on)
import qualified Data.List                   as L
import           Data.Maybe                  (fromMaybe)
import           Data.Serialize              as S (Serialize, get, put, runGet, runPut)
import qualified Data.Text                   as T
import           Data.Time                   (getCurrentTime)
import qualified Database.Esqueleto          as E
import           Database.Persist
import           Database.Persist.Postgresql (SqlBackend)

import           Experimenter.Experiment
import           Experimenter.Input
import           Experimenter.Measure
import           Experimenter.Models
import           Experimenter.Parameter
import           Experimenter.Result.Type
import           Experimenter.Setup
import           Experimenter.StepResult
import           Experimenter.Util


loadExperiment :: (ExperimentDef a, MonadLogger m, MonadIO m) => ExperimentSetup -> InputState a -> a -> ReaderT SqlBackend m (Experiment a)
loadExperiment setup initInpSt initSt = do
  eExp <- getOrCreateExp setup initInpSt initSt
  eSetup <- fromMaybe (error "Setup not found. Your DB is corrupted!") <$> getBy (UniqueExpSetup (entityKey eExp))
  let e = entityVal eExp
  expRes <- loadExperimentResults (entityKey eExp)
  return $ Experiment (entityKey eExp) (view expName e) (view expStartTime e) (view expEndTime e) (entityVal eSetup) (parameters initSt) initSt initInpSt expRes


deserialise :: (MonadLogger m, Serialize a) => ByteString -> m (Maybe a)
deserialise bs =
  let res = runGet S.get bs
  in case res of
    Left err  -> $(logError) ("Could not deserialise data! Regarding saved experiment result." <> tshow err) >> return Nothing
    Right r -> return $ Just r


loadExperimentResults :: (ExperimentDef a, MonadLogger m, MonadIO m) => Key Exp -> ReaderT SqlBackend m [ExperimentResult a]
loadExperimentResults kExp = do
  xs <- selectList [ExpResultExp ==. kExp] []
  fromMaybe [] . sequence <$> mapM loadExperimentResult xs


loadExperimentResult :: (ExperimentDef a, MonadLogger m, MonadIO m) => Entity ExpResult -> ReaderT SqlBackend m (Maybe (ExperimentResult a))
loadExperimentResult (Entity k (ExpResult _ rep prepStartT prepEndT prepEndStBS prepEndInpStBS)) = do
  paramSetting <- loadParamSetup k
  mPrepInputVals <- loadPreparationInput k
  prepResults <- loadPrepartionMeasures k
  evalResults <- loadReplicationResults k
  mPrepEndInpSt <- deserialise prepEndInpStBS
  mPrepEndSt <- deserialise prepEndStBS
  return $
    do prepEndInpSt <- mPrepEndInpSt
       prepEndSt <- mPrepEndSt
       prepInputVals <- mPrepInputVals
       return $ ExperimentResult (Just k) rep paramSetting prepStartT prepEndT prepInputVals prepResults prepEndSt prepEndInpSt evalResults


loadParamSetup :: (MonadLogger m, MonadIO m) => Key ExpResult -> ReaderT SqlBackend m [ParameterSetting a]
loadParamSetup kExp = map (mkParameterSetting' . entityVal) <$> selectList [ParamSettingExpResult ==. kExp] []
  where
    mkParameterSetting' (ParamSetting _ n v) = ParameterSetting n v


loadPreparationInput :: (ExperimentDef a, MonadLogger m, MonadIO m) => Key ExpResult -> ReaderT SqlBackend m (Maybe [Input a])
loadPreparationInput kExpRes = do
  res <-
    E.select $
    E.from $ \(prepI, prepIV) -> do
      E.where_ (prepI E.^. PrepInputId E.==. prepIV E.^. PrepInputValuePrepInput)
      E.where_ (prepI E.^. PrepInputExpResult E.==. E.val kExpRes)

      return (prepI, prepIV)
  sequence <$> mapM mkInput res
  where
    mkInput :: (MonadLogger m, ExperimentDef a) => (Entity PrepInput, Entity PrepInputValue) -> m (Maybe (Input a))
    mkInput (Entity _ (PrepInput _ p), Entity _ (PrepInputValue _ v)) = do
      v' <- deserialise v
      return $ Input p <$> v'


loadPrepartionMeasures :: (MonadLogger m, MonadIO m) => Key ExpResult -> ReaderT SqlBackend m [Measure]
loadPrepartionMeasures kExpRes = do
  res <-
    E.select $
    E.from $ \(prepM, prepRS) -> do
      E.where_ (prepM E.^. PrepMeasureId E.==. prepRS E.^. PrepResultStepMeasure)
      E.where_ (prepM E.^. PrepMeasureExpResult E.==. E.val kExpRes)
      E.orderBy [E.asc (prepM E.^. PrepMeasurePeriod)]
      return (prepM, prepRS)
  return $ map combineMeasures $ L.groupBy ((==) `on` view measurePeriod) $ map mkMeasure res
  where
    mkMeasure (Entity _ (PrepMeasure _ p), Entity _ (PrepResultStep _ n x y)) = Measure p [StepResult n x y]
    combineMeasures xs@(Measure p _:_) = Measure p (concatMap (view measureResults) xs)
    combineMeasures _                  = error "not possible"

loadReplicationResults :: (ExperimentDef a, MonadLogger m, MonadIO m) => Key ExpResult -> ReaderT SqlBackend m [ReplicationResult a]
loadReplicationResults kExpRes = do
  xs <- selectList [RepResultExpResult ==. kExpRes] []
  maybe [] (L.sortBy (compare `on` view replicationNumber)) . sequence <$> mapM loadReplicationResult xs


loadReplicationResult :: (ExperimentDef a, MonadLogger m, MonadIO m) => Entity RepResult -> ReaderT SqlBackend m (Maybe (ReplicationResult a))
loadReplicationResult (Entity k (RepResult _ repNr randGen wmUpEndStBS wmUpEndInpStBS wmUpEndTime repEndStBS repEndInpStBS repEndTime)) = do
  mWmUpInpVals <- loadReplicationWarmUpInput k
  wmUpMeasures <- loadReplicationWarmUpMeasures k
  mWmUpEndSt <- deserialise wmUpEndStBS
  mWmUpEndInpSt <- deserialise wmUpEndInpStBS
  mRepInpVals <- loadReplicationInput k
  repMeasures <- loadReplicationMeasures k
  mRepEndSt <- deserialise repEndStBS
  mRepEndInpSt <- deserialise repEndInpStBS
  return $ do
    wmUpEndSt <- mWmUpEndSt
    wmUpEndInpSt <- mWmUpEndInpSt
    wmUpInpVals <- mWmUpInpVals
    repEndSt <- mRepEndSt
    repEndInpSt <- mRepEndInpSt
    repInpVals <- mRepInpVals
    return $
      ReplicationResult (Just k) repNr (read $ T.unpack randGen) wmUpInpVals wmUpMeasures wmUpEndSt wmUpEndInpSt wmUpEndTime repInpVals repMeasures repEndSt repEndInpSt repEndTime


loadReplicationWarmUpInput :: (ExperimentDef a, MonadLogger m, MonadIO m) => Key RepResult -> ReaderT SqlBackend m (Maybe [Input a])
loadReplicationWarmUpInput kExpRes = do
  res <-
    E.select $
    E.from $ \(warmUpI, warmUpIV) -> do
      E.where_ (warmUpI E.^. WarmUpInputId E.==. warmUpIV E.^. WarmUpInputValueWarmUpInput)
      E.where_ (warmUpI E.^. WarmUpInputRepResult E.==. E.val kExpRes)
      return (warmUpI, warmUpIV)
  sequence <$> mapM mkInput res
  where
    mkInput (Entity _ (WarmUpInput _ p), Entity _ (WarmUpInputValue _ v)) = do
      v' <- deserialise v
      return $ Input p <$> v'


loadReplicationWarmUpMeasures :: (MonadLogger m, MonadIO m) => Key RepResult -> ReaderT SqlBackend m [Measure]
loadReplicationWarmUpMeasures kExpRes = do
  res <-
    E.select $
    E.from $ \(warmUpM, warmUpRS) -> do
      E.where_ (warmUpM E.^. WarmUpMeasureId E.==. warmUpRS E.^. WarmUpResultStepMeasure)
      E.where_ (warmUpM E.^. WarmUpMeasureRepResult E.==. E.val kExpRes)
      E.orderBy [E.asc (warmUpM E.^. WarmUpMeasurePeriod)]
      return (warmUpM, warmUpRS)
  return $ map combineMeasures $ L.groupBy ((==) `on` view measurePeriod) $ map mkMeasure res
  where
    mkMeasure (Entity _ (WarmUpMeasure _ p), Entity _ (WarmUpResultStep _ n x y)) = Measure p [StepResult n x y]
    combineMeasures xs@(Measure p _:_) = Measure p (concatMap (view measureResults) xs)
    combineMeasures _                  = error "not possible"


loadReplicationInput :: (ExperimentDef a, MonadLogger m, MonadIO m) => Key RepResult -> ReaderT SqlBackend m (Maybe [Input a])
loadReplicationInput kExpRes = do
  res <-
    E.select $
    E.from $ \(repI, repIV) -> do
      E.where_ (repI E.^. RepInputId E.==. repIV E.^. RepInputValueRepInput)
      E.where_ (repI E.^. RepInputRepResult E.==. E.val kExpRes)
      return (repI, repIV)
  sequence <$> mapM mkInput res
  where
    mkInput (Entity _ (RepInput _ p), Entity _ (RepInputValue _ v)) = do
      v' <- deserialise v
      return $ Input p <$> v'


loadReplicationMeasures :: (MonadLogger m, MonadIO m) => Key RepResult -> ReaderT SqlBackend m [Measure]
loadReplicationMeasures kExpRes = do
  res <-
    E.select $
    E.from $ \(repM, repRS) -> do
      E.where_ (repM E.^. RepMeasureId E.==. repRS E.^. RepResultStepMeasure)
      E.where_ (repM E.^. RepMeasureRepResult E.==. E.val kExpRes)
      E.orderBy [E.asc (repM E.^. RepMeasurePeriod)]
      return (repM, repRS)
  return $ map combineMeasures $ L.groupBy ((==) `on` view measurePeriod) $ map mkMeasure res
  where
    mkMeasure (Entity _ (RepMeasure _ p), Entity _ (RepResultStep _ n x y)) = Measure p [StepResult n x y]
    combineMeasures xs@(Measure p _:_) = Measure p (concatMap (view measureResults) xs)
    combineMeasures _                  = error "not possible"


getOrCreateExp :: (ExperimentDef a, MonadLogger m, MonadIO m) => ExperimentSetup -> InputState a -> a -> ReaderT SqlBackend m (Entity Exp)
getOrCreateExp setup initInpSt initSt = do
  let name = view experimentBaseName setup
  exps <- selectList [ExpName ==. name, ExpInitialInputState ==. runPut (put initInpSt), ExpInitialState ==. runPut (put initSt)] []
  params <- mapM (\e -> selectList [ParamExp ==. entityKey e] []) exps
  let mkParamTpl (Param _ n minB maxB) = (n, minB, maxB)
  let myParams = map (mkParamTpl . convertParameterSetup (error "Ref not used")) (parameters initSt)
  case L.find ((== myParams) . map (mkParamTpl . entityVal) . snd) (zip exps params) of
    Nothing -> do
      $(logInfo) "Starting new experiment..."
      time <- liftIO getCurrentTime
      eExp <- insertEntity $ Exp name time Nothing (runPut $ put initSt) (runPut $ put initInpSt)
      void $ insert $ mkExpSetup eExp
      return eExp
    Just (eExp, _) -> do
      $(logInfo) "Found experiment with same name and parameter settings. Continuing experiment ..."
      return eExp
  where
    mkExpSetup eExp =
      ExpSetup
        (entityKey eExp)
        (view experimentRepetitions setup)
        (view preparationSteps setup)
        (view evaluationWarmUpSteps setup)
        (view evaluationSteps setup)
        (view evaluationReplications setup)
        (view maximumParallelEvaluations setup)
