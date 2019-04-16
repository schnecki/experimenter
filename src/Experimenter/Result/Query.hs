{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Experimenter.Result.Query
    ( loadExperiments
    ) where


import           Control.Lens                (view)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B
import           Data.Function               (on)
import qualified Data.List                   as L
import           Data.Maybe                  (fromMaybe, isJust)
import           Data.Serialize              as S (Serialize, get, put, runGet, runPut)
import qualified Data.Text                   as T
import           Data.Time                   (getCurrentTime)
import qualified Database.Esqueleto          as E
import           Database.Persist
import           Database.Persist.Postgresql (SqlBackend)
import           GHC.Stack
import           System.Random               (newStdGen)

import           Experimenter.Experiment
import           Experimenter.Input
import           Experimenter.Measure
import           Experimenter.Models
import           Experimenter.Parameter
import           Experimenter.Result.Type
import           Experimenter.Setup
import           Experimenter.StepResult
import           Experimenter.Util


loadExperiments :: (ExperimentDef a, MonadLogger m, MonadIO m) => ExperimentSetup -> InputState a -> a -> ReaderT SqlBackend m (Experiments a)
loadExperiments setup initInpSt initSt = do
  eExp <- getOrCreateExps setup initInpSt initSt
  let e = entityVal eExp
  exps <- L.sortBy (compare `on` view experimentNumber) <$> loadExperimentList (entityKey eExp)
  eSetup <- fromMaybe (error "Setup not found. Your DB is corrupted!") <$> getBy (UniqueExpsSetup (entityKey eExp))
  return $ Experiments (entityKey eExp) (view expsName e) (view expsStartTime e) (view expsEndTime e) (entityVal eSetup) (parameters initSt) initSt initInpSt exps


loadExperimentList :: (ExperimentDef a, MonadLogger m, MonadIO m) => Key Exps -> ReaderT SqlBackend m [Experiment a]
loadExperimentList expsKey = selectList [ExpExps ==. expsKey] [] >>= mapM mkExperiment
    where mkExperiment (Entity k exp) = do
            paramSetting <- loadParamSetup k
            Experiment k (view expNumber exp) (view expStartTime exp) (view expEndTime exp) paramSetting <$> loadExperimentResults k


mDeserialise :: (MonadIO m, MonadLogger m ,Serialize a) => T.Text -> Maybe ByteString -> m (Maybe (Maybe a))
mDeserialise n mBs = sequence (deserialise n <$> mBs)


deserialise :: (MonadIO m, MonadLogger m, Serialize a) => T.Text -> ByteString -> m (Maybe a)
deserialise n bs =
  let res = runGet S.get bs
   in case res of
        Left err -> do
          $(logError) $ "Could not deserialise " <> n <> "! Discarding saved experiment result. Data length: " <> tshow (B.length bs) <> ". Error Message: " <> tshow err
          return Nothing
        Right r -> return $ Just r


loadExperimentResults :: (ExperimentDef a, MonadLogger m, MonadIO m) => Key Exp -> ReaderT SqlBackend m [ExperimentResult a]
loadExperimentResults kExp = do
  xs <- selectList [ExpResultExp ==. kExp] []
  mapM loadExperimentResult xs


loadExperimentResult :: (ExperimentDef a, MonadLogger m, MonadIO m) => Entity ExpResult -> ReaderT SqlBackend m (ExperimentResult a)
loadExperimentResult (Entity k (ExpResult _ rep)) = do
  mEPrepResData <- getBy (UniquePrepResultDataExpResult k)
  prepRes <- case mEPrepResData of
    Nothing -> return Nothing
    Just (Entity resDataKey (PrepResultData _ startT endT startRandGen endRandGen startStBS endStBS startInpStBS endInpStBS)) -> do
      mInputVals <- loadPreparationInput k
      results <- loadPrepartionMeasures k
      mStartSt <- deserialise "prep start state" startStBS
      mEndSt <- mDeserialise "prep end state" endStBS
      mStartInpSt <- deserialise "prep start input state" startInpStBS
      mEndInpSt <- mDeserialise "prep end input state" endInpStBS
      return $ do
        startSt <- mStartSt
        endSt <- mEndSt
        endInpSt <- mEndInpSt
        startInpSt <- mStartInpSt
        inputVals <- mInputVals
        return $ ResultData (ResultDataPrep resDataKey) startT endT (tread startRandGen) (tread <$> endRandGen) inputVals results startSt endSt startInpSt endInpSt
  evalResults <- loadReplicationResults k
  return $ ExperimentResult k rep prepRes evalResults

loadParamSetup :: (MonadLogger m, MonadIO m) => Key Exp -> ReaderT SqlBackend m [ParameterSetting a]
loadParamSetup kExp = L.sortBy (compare `on` view parameterSettingName) . map (mkParameterSetting' . entityVal) <$> selectList [ParamSettingExp ==. kExp] []
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
    mkInput :: (MonadIO m, MonadLogger m, ExperimentDef a) => (Entity PrepInput, Entity PrepInputValue) -> m (Maybe (Input a))
    mkInput (Entity _ (PrepInput _ p), Entity _ (PrepInputValue _ v)) = do
      v' <- deserialise "prep input value" v
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
  L.sortBy (compare `on` view replicationNumber) <$> mapM loadReplicationResult xs


loadReplicationResult :: (ExperimentDef a, MonadLogger m, MonadIO m) => Entity RepResult -> ReaderT SqlBackend m (ReplicationResult a)
loadReplicationResult (Entity k (RepResult _ repNr)) = do
  mWmUpRes <- getBy (UniqueWarmUpResultDataRepResult k)
  mRepRes <- getBy (UniqueRepResultDataRepResult k)
  wmUp <-
    case mWmUpRes of
      Nothing    -> return Nothing
      Just eWmUp -> mkWmUp eWmUp
  rep <-
    case mRepRes of
      Nothing   -> return Nothing
      Just eRep -> mkRep eRep
  return $ ReplicationResult k repNr wmUp rep
  where
    mkWmUp (Entity wmUpResKey wmUpRes) = do
      let wmUpStartTime = view warmUpResultDataStartTime wmUpRes
      let wmUpEndTime = view warmUpResultDataEndTime wmUpRes
      let wmUpStartRandGen = tread $ view warmUpResultDataStartRandGen wmUpRes
      let wmUpEndRandGen = tread <$> view warmUpResultDataEndRandGen wmUpRes
      mWmUpInpVals <- loadReplicationWarmUpInput k
      wmUpMeasures <- loadReplicationWarmUpMeasures k
      mWmUpStartSt <- deserialise "warm up start state" (view warmUpResultDataStartState wmUpRes)
      mWmUpEndSt <- mDeserialise "warm up end state" (view warmUpResultDataEndState wmUpRes)
      mWmUpStartInpSt <- deserialise "warm up start input state" (view warmUpResultDataStartInputState wmUpRes)
      mWmUpEndInpSt <- mDeserialise "warm up end input state" (view warmUpResultDataEndInputState wmUpRes)
      return $ do
        wmUpInpVals <- mWmUpInpVals
        wmUpStartSt <- mWmUpStartSt
        wmUpEndSt <- mWmUpEndSt
        wmUpStartInpSt <- mWmUpStartInpSt
        wmUpEndInpSt <- mWmUpEndInpSt
        return $
          ResultData
            (ResultDataWarmUp wmUpResKey)
            wmUpStartTime
            wmUpEndTime
            wmUpStartRandGen
            wmUpEndRandGen
            wmUpInpVals
            wmUpMeasures
            wmUpStartSt
            wmUpEndSt
            wmUpStartInpSt
            wmUpEndInpSt
    mkRep (Entity repResKey repRes) = do
      let repStartTime = view repResultDataStartTime repRes
      let repEndTime = view repResultDataEndTime repRes
      let repStartRandGen = tread $ view repResultDataStartRandGen repRes
      let repEndRandGen = tread <$> view repResultDataEndRandGen repRes
      mRepInpVals <- loadReplicationInput k
      repMeasures <- loadReplicationMeasures k
      mRepStartSt <- deserialise "rep start state" (view repResultDataStartState repRes)
      mRepEndSt <- mDeserialise "rep end state" (view repResultDataEndState repRes)
      mRepStartInpSt <- deserialise "rep start input state" (view repResultDataStartInputState repRes)
      mRepEndInpSt <- mDeserialise "rep end input state" (view repResultDataEndInputState repRes)
      return $ do
        repInpVals <- mRepInpVals
        repStartSt <- mRepStartSt
        repEndSt <- mRepEndSt
        repStartInpSt <- mRepStartInpSt
        repEndInpSt <- mRepEndInpSt
        return $ ResultData (ResultDataRep repResKey) repStartTime repEndTime repStartRandGen repEndRandGen repInpVals repMeasures repStartSt repEndSt repStartInpSt repEndInpSt


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
      v' <- deserialise "warm up input value" v
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
      v' <- deserialise "eval input value" v
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

getOrCreateExps :: forall m a . (ExperimentDef a, MonadLogger m, MonadIO m) => ExperimentSetup -> InputState a -> a -> ReaderT SqlBackend m (Entity Exps)
getOrCreateExps setup initInpSt initSt = do
  let name = view experimentBaseName setup
  -- expsList <- selectList [ExpsName ==. name, ExpsInitialInputState ==. runPut (put initInpSt), ExpsInitialState ==. runPut (put initSt)] []
  expsList <- selectList [ExpsName ==. name] []
  let exps =
        filter
          (\(Entity _ (Exps _ _ _ s iS)) ->
             let other = (,) <$> runGet S.get s <*> runGet S.get iS
             in fromEither False (equalExperiments (initSt, initInpSt) <$> other))
          expsList
  params <- mapM (\e -> selectList [ParamExps ==. entityKey e] [Asc ParamName]) exps
  let mkParamTpl (Param _ n minB maxB) = (n, minB, maxB)
  let myParams = L.sortBy (compare `on` (\(x, _, _) -> x)) $ map (mkParamTpl . convertParameterSetup (error "Ref not used")) (parameters initSt)
  case L.find ((== myParams) . map (mkParamTpl . entityVal) . snd) (zip exps params) of
    Nothing -> do
      $(logInfo) "Starting new experiment..."
      time <- liftIO getCurrentTime
      eExp <- insertEntity $ Exps name time Nothing (runPut $ put initSt) (runPut $ put initInpSt)
      void $ insert $ mkExpSetup eExp
      return eExp
    Just (eExp, _) -> do
      $(logInfo) "Found experiment with same name and parameter settings. Continuing experiment ..."
      putMany [mkExpSetup eExp]
      return eExp
  where
    mkExpSetup eExp =
      ExpsSetup
        (entityKey eExp)
        (max 1 $ view experimentRepetitions setup)
        (max 0 $ view preparationSteps setup)
        (max 0 $ view evaluationWarmUpSteps setup)
        (max 0 $ view evaluationSteps setup)
        (max 1 $ view evaluationReplications setup)
        (max 1 $ view maximumParallelEvaluations setup)
