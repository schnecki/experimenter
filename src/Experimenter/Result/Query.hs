{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Experimenter.Result.Query
    ( loadExperiments
    , loadPrepartionMeasures
    , loadPreparationInput
    , loadReplicationWarmUpMeasures
    , loadReplicationWarmUpInput
    , loadReplicationInput
    , loadReplicationMeasures
    ) where


import           Control.Lens                         (view)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString                      as B
import           Data.Function                        (on)
import qualified Data.List                            as L
import           Data.Maybe                           (fromMaybe, listToMaybe)
import           Data.Serialize                       as S (Serialize, get, put, runGet,
                                                            runPut)
import qualified Data.Text                            as T
import           Data.Time                            (getCurrentTime)
import qualified Database.Esqueleto                   as E
import qualified Database.Esqueleto.Internal.Language as E
import           Database.Persist                     as P
import           Database.Persist.Postgresql          (SqlBackend)
import           System.Exit                          (exitFailure)

import           Experimenter.Experiment
import           Experimenter.Input
import           Experimenter.Measure
import           Experimenter.Models
import           Experimenter.Parameter
import           Experimenter.Result.Type
import           Experimenter.Setup
import           Experimenter.StepResult
import           Experimenter.Util


import           Debug.Trace

-- selectCount :: (MonadIO m) => (E.From E.SqlQuery E.SqlExpr SqlBackend a) => (a -> E.SqlQuery ()) -> ReaderT SqlBackend m Int
-- selectCount q = do
--   res <- E.select $ E.from (\x -> q x >> return E.countRows)
--   return $ fromMaybe 0 . listToMaybe . fmap (\(E.Value v) -> v) $ res

loadExperiments :: (ExperimentDef a) => ExperimentSetup -> InputState a -> a -> ReaderT SqlBackend (LoggingT (ExpM a)) (Experiments a)
loadExperiments setup initInpSt initSt = do
  eExp <- getOrCreateExps setup initInpSt initSt
  let e = entityVal eExp
  exps <- L.sortBy (compare `on` view experimentNumber) <$> loadExperimentList (entityKey eExp)
  eSetup <- fromMaybe (error "Setup not found. Your DB is corrupted!") <$> getBy (UniqueExpsSetup (entityKey eExp))
  return $ Experiments (entityKey eExp) (view expsName e) (view expsStartTime e) (view expsEndTime e) (entityVal eSetup) (parameters initSt) initSt initInpSt exps


loadExperimentList :: (ExperimentDef a) => Key Exps -> ReaderT SqlBackend (LoggingT (ExpM a)) [Experiment a]
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
          liftIO exitFailure
          -- return Nothing
        Right r -> return $ Just r


loadExperimentResults :: (ExperimentDef a) => Key Exp -> ReaderT SqlBackend (LoggingT (ExpM a)) [ExperimentResult a]
loadExperimentResults kExp = do
  xs <- selectList [ExpResultExp ==. kExp] []
  mapM loadExperimentResult xs


loadExperimentResult :: forall a m . (ExperimentDef a) => Entity ExpResult -> ReaderT SqlBackend (LoggingT (ExpM a)) (ExperimentResult a)
loadExperimentResult (Entity k (ExpResult _ rep mPrepResDataId)) = do
  mEPrepResData <- fmap join $ sequence $ P.getEntity <$> mPrepResDataId
  prepRes <- case mEPrepResData of
    Nothing -> return Nothing
    Just (Entity resDataKey (PrepResultData startT endT startRandGen endRandGen startStBS endStBS startInpStBS endInpStBS)) -> do
      mStartSt <- deserialise "prep start state" startStBS >>= lift . lift . sequence . fmap deserialisable
      mEndSt <- mDeserialise "prep end state" endStBS >>= lift . lift . sequence . fmap sequence . fmap (fmap deserialisable)
      mStartInpSt <- deserialise "prep start input state" startInpStBS
      mEndInpSt <- mDeserialise "prep end input state" endInpStBS
      inpCount <- loadPreparationInputCount resDataKey
      resultCount <- loadPrepartionMeasuresCount resDataKey
      return $ do
        startSt <- mStartSt
        endSt <- mEndSt
        endInpSt <- mEndInpSt
        startInpSt <- mStartInpSt
        -- inputVals <- mInputVals
        let inputVals = AvailableFromDB (fromMaybe [] <$> loadPreparationInput resDataKey)
        let results = AvailableFromDB (loadPrepartionMeasures resDataKey)
        return $ ResultData (ResultDataPrep resDataKey) startT endT (tread startRandGen) (tread <$> endRandGen) (inpCount, inputVals) (resultCount, results) startSt endSt startInpSt endInpSt
  evalResults <- loadReplicationResults k
  return $ ExperimentResult k rep prepRes evalResults

loadParamSetup :: (MonadLogger m, MonadIO m) => Key Exp -> ReaderT SqlBackend m [ParameterSetting a]
loadParamSetup kExp = L.sortBy (compare `on` view parameterSettingName) . map (mkParameterSetting' . entityVal) <$> selectList [ParamSettingExp ==. kExp] []
  where
    mkParameterSetting' (ParamSetting _ n v b design) = ParameterSetting n v b (toEnum design)

fromCount :: [E.Value Int] -> Int
fromCount = fromMaybe 0 . listToMaybe . fmap (\(E.Value v) -> v)

loadPreparationInputCount  :: (MonadIO m) => Key PrepResultData -> ReaderT SqlBackend m Int
loadPreparationInputCount kExpRes =
  fmap fromCount $ E.select $ E.from $ \(prepI, prepIV) -> do
    E.where_ (prepI E.^. PrepInputId E.==. prepIV E.^. PrepInputValuePrepInput)
    E.where_ (prepI E.^. PrepInputPrepResultData E.==. E.val kExpRes)
    return E.countRows

loadPreparationInput :: (ExperimentDef a) => Key PrepResultData -> ReaderT SqlBackend (LoggingT (ExpM a)) (Maybe [Input a])
loadPreparationInput kExpRes = do
  res <-
    E.select $
    E.from $ \(prepI, prepIV) -> do
      E.where_ (prepI E.^. PrepInputId E.==. prepIV E.^. PrepInputValuePrepInput)
      E.where_ (prepI E.^. PrepInputPrepResultData E.==. E.val kExpRes)
      return (prepI, prepIV)
  sequence <$> mapM mkInput res
  where
    mkInput :: (MonadIO m, MonadLogger m, ExperimentDef a) => (Entity PrepInput, Entity PrepInputValue) -> m (Maybe (Input a))
    mkInput (Entity _ (PrepInput _ p), Entity _ (PrepInputValue _ v)) = do
      v' <- deserialise "prep input value" v
      return $ Input p <$> v'

loadPrepartionMeasuresCount :: (MonadIO m) => Key PrepResultData -> ReaderT SqlBackend m Int
loadPrepartionMeasuresCount kExpRes =
  fmap fromCount $ E.select $ E.from $ \(prepM, prepRS) -> do
      E.where_ (prepM E.^. PrepMeasureId E.==. prepRS E.^. PrepResultStepMeasure)
      E.where_ (prepM E.^. PrepMeasurePrepResultData E.==. E.val kExpRes)
      return E.countRows

loadPrepartionMeasures :: (MonadLogger m, MonadIO m) => Key PrepResultData -> ReaderT SqlBackend m [Measure]
loadPrepartionMeasures kExpRes = do
  res <-
    E.select $
    E.from $ \(prepM, prepRS) -> do
      E.where_ (prepM E.^. PrepMeasureId E.==. prepRS E.^. PrepResultStepMeasure)
      E.where_ (prepM E.^. PrepMeasurePrepResultData E.==. E.val kExpRes)
      E.orderBy [E.asc (prepM E.^. PrepMeasurePeriod)]
      return (prepM, prepRS)
  return $ map combineMeasures $ L.groupBy ((==) `on` view measurePeriod) $ map mkMeasure res
  where
    mkMeasure (Entity _ (PrepMeasure _ p), Entity _ (PrepResultStep _ n x y)) = Measure p [StepResult n x y]
    combineMeasures xs@(Measure p _:_) = Measure p (concatMap (view measureResults) xs)
    combineMeasures _                  = error "not possible"

loadReplicationResults :: (ExperimentDef a) => Key ExpResult -> ReaderT SqlBackend (LoggingT (ExpM a)) [ReplicationResult a]
loadReplicationResults kExpRes = do
  xs <- selectList [RepResultExpResult ==. kExpRes] []
  L.sortBy (compare `on` view replicationNumber) <$> mapM loadReplicationResult xs


loadReplicationResult :: (ExperimentDef a) => Entity RepResult -> ReaderT SqlBackend (LoggingT (ExpM a)) (ReplicationResult a)
loadReplicationResult (Entity k (RepResult _ repNr mWmUpResId mRepResId)) = do
  mWmUpRes <- fmap join $ sequence $ P.getEntity <$> mWmUpResId
  mRepRes <- fmap join $ sequence $ P.getEntity <$> mRepResId
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
      -- mWmUpInpVals <- loadReplicationWarmUpInput wmUpResKey
      -- wmUpMeasures <- loadReplicationWarmUpMeasures wmUpResKey
      mWmUpStartSt <- deserialise "warm up start state" (view warmUpResultDataStartState wmUpRes) >>= lift . lift . sequence . fmap deserialisable
      mWmUpEndSt <- mDeserialise "warm up end state" (view warmUpResultDataEndState wmUpRes) >>= lift . lift . sequence . fmap sequence . fmap (fmap deserialisable)
      mWmUpStartInpSt <- deserialise "warm up start input state" (view warmUpResultDataStartInputState wmUpRes)
      mWmUpEndInpSt <- mDeserialise "warm up end input state" (view warmUpResultDataEndInputState wmUpRes)
      wmUpInpValsCount <- loadReplicationWarmUpInputCount wmUpResKey
      wmUpMeasuresCount <- loadReplicationWarmUpMeasuresCount wmUpResKey
      return $ do
        let wmUpInpVals = AvailableFromDB (fromMaybe [] <$> loadReplicationWarmUpInput wmUpResKey)
            wmUpMeasures = AvailableFromDB (loadReplicationWarmUpMeasures wmUpResKey)
        -- wmUpInpVals <- mWmUpInpVals
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
            (wmUpInpValsCount, wmUpInpVals)
            (wmUpMeasuresCount, wmUpMeasures)
            wmUpStartSt
            wmUpEndSt
            wmUpStartInpSt
            wmUpEndInpSt
    mkRep (Entity repResKey repRes) = do
      let repStartTime = view repResultDataStartTime repRes
      let repEndTime = view repResultDataEndTime repRes
      let repStartRandGen = tread $ view repResultDataStartRandGen repRes
      let repEndRandGen = tread <$> view repResultDataEndRandGen repRes
      -- mRepInpVals <- loadReplicationInput repResKey
      -- repMeasures <- loadReplicationMeasures repResKey
      mRepStartSt <- deserialise "rep start state" (view repResultDataStartState repRes) >>= lift . lift . sequence . fmap deserialisable
      mRepEndSt <- mDeserialise "rep end state" (view repResultDataEndState repRes) >>= lift . lift . sequence . fmap sequence . fmap (fmap deserialisable)
      mRepStartInpSt <- deserialise "rep start input state" (view repResultDataStartInputState repRes)
      mRepEndInpSt <- mDeserialise "rep end input state" (view repResultDataEndInputState repRes)
      repInpValsCount <- loadReplicationInputCount repResKey
      repMeasuresCount <- loadReplicationMeasuresCount repResKey
      return $
        -- repInpVals <- mRepInpVals
       do
        let repInpVals = AvailableFromDB (fromMaybe [] <$> loadReplicationInput repResKey)
            repMeasures = AvailableFromDB (loadReplicationMeasures repResKey)
        repStartSt <- mRepStartSt
        repEndSt <- mRepEndSt
        repStartInpSt <- mRepStartInpSt
        repEndInpSt <- mRepEndInpSt
        return $
          ResultData
            (ResultDataRep repResKey)
            repStartTime
            repEndTime
            repStartRandGen
            repEndRandGen
            (repInpValsCount, repInpVals)
            (repMeasuresCount, repMeasures)
            repStartSt
            repEndSt
            repStartInpSt
            repEndInpSt


loadReplicationWarmUpInputCount :: (MonadIO m) => Key WarmUpResultData -> ReaderT SqlBackend m Int
loadReplicationWarmUpInputCount kExpRes =
  fmap fromCount $ E.select $ E.from $ \(warmUpI, warmUpIV) -> do
    E.where_ (warmUpI E.^. WarmUpInputId E.==. warmUpIV E.^. WarmUpInputValueWarmUpInput)
    E.where_ (warmUpI E.^. WarmUpInputRepResult E.==. E.val kExpRes)
    return E.countRows

loadReplicationWarmUpInput :: (ExperimentDef a, MonadLogger m, MonadIO m) => Key WarmUpResultData -> ReaderT SqlBackend m (Maybe [Input a])
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


loadReplicationWarmUpMeasuresCount :: (MonadIO m) => Key WarmUpResultData -> ReaderT SqlBackend m Int
loadReplicationWarmUpMeasuresCount kExpRes =
  fmap fromCount $ E.select $ E.from $ \(warmUpM, warmUpRS) -> do
    E.where_ (warmUpM E.^. WarmUpMeasureId E.==. warmUpRS E.^. WarmUpResultStepMeasure)
    E.where_ (warmUpM E.^. WarmUpMeasureRepResult E.==. E.val kExpRes)
    return E.countRows

loadReplicationWarmUpMeasures :: (MonadLogger m, MonadIO m) => Key WarmUpResultData -> ReaderT SqlBackend m [Measure]
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

loadReplicationInputCount :: (MonadIO m) => Key RepResultData -> ReaderT SqlBackend m Int
loadReplicationInputCount kExpRes =
  fmap fromCount $ E.select $ E.from $ \(repI, repIV) -> do
    E.where_ (repI E.^. RepInputId E.==. repIV E.^. RepInputValueRepInput)
    E.where_ (repI E.^. RepInputRepResult E.==. E.val kExpRes)
    return E.countRows

loadReplicationInput :: (ExperimentDef a, MonadLogger m, MonadIO m) => Key RepResultData -> ReaderT SqlBackend m (Maybe [Input a])
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


loadReplicationMeasuresCount :: (MonadIO m) => Key RepResultData -> ReaderT SqlBackend m Int
loadReplicationMeasuresCount kExpRes =
  fmap fromCount $ E.select $ E.from $ \(repM, repRS) -> do
    E.where_ (repM E.^. RepMeasureId E.==. repRS E.^. RepResultStepMeasure)
    E.where_ (repM E.^. RepMeasureRepResult E.==. E.val kExpRes)
    return E.countRows


loadReplicationMeasures :: (MonadLogger m, MonadIO m) => Key RepResultData -> ReaderT SqlBackend m [Measure]
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

getOrCreateExps :: forall a . (ExperimentDef a) => ExperimentSetup -> InputState a -> a -> ReaderT SqlBackend (LoggingT (ExpM a)) (Entity Exps)
getOrCreateExps setup initInpSt initSt = do
  let name = view experimentBaseName setup
  -- expsList <- selectList [ExpsName ==. name, ExpsInitialInputState ==. runPut (put initInpSt), ExpsInitialState ==. runPut (put initSt)] []
  expsList <- selectList [ExpsName ==. name] []
  exps <-
    filterM
      (\(Entity _ (Exps _ _ _ s iS)) -> do
          serSt <- lift $ lift $ sequence $ deserialisable <$> (runGet S.get s)
          let other = (,) <$> serSt <*> runGet S.get iS
          return $ fromEither False (equalExperiments (initSt, initInpSt) <$> other))
      expsList
  params <- mapM (\e -> selectList [ParamExps ==. entityKey e] [Asc ParamName]) exps
  let mkParamTpl (Param _ n minB maxB) = (n, minB, maxB)
  let myParams = L.sortBy (compare `on` (\(x, _, _) -> x)) $ map (mkParamTpl . convertParameterSetup (entityKey (head exps))) (parameters initSt)
  case L.find ((== myParams) . map (mkParamTpl . entityVal) . snd) (zip exps params) of
    Nothing -> do
      $(logInfo) "Starting new experiment..."
      time <- trace ("put") $ liftIO getCurrentTime
      serInitSt <- trace ("ser") $ lift $ lift $ serialisable initSt
      eExp <- trace ("insert") $ insertEntity $ Exps name time Nothing (runPut $ put serInitSt) (runPut $ put initInpSt)
      void $ insert $ mkExpSetup eExp
      mapM_ (insertParam (entityKey eExp)) (parameters initSt)
      trace ("DONE insert") $ $(logInfo) "ASDF"
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
    insertParam :: Key Exps -> ParameterSetup a -> ReaderT SqlBackend (LoggingT (ExpM a)) (Key Param)
    insertParam eExp (ParameterSetup n _ _ _ (Just (minVal, maxVal)) drp _) = insert $ Param eExp n (Just $ runPut $ put minVal) (Just $ runPut $ put maxVal)
    insertParam eExp (ParameterSetup n _ _ _ Nothing drp _) = insert $ Param eExp n Nothing Nothing
