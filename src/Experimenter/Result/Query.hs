{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
module Experimenter.Result.Query
    ( loadExperiments
    , loadPrepartionMeasures
    , loadPreparationInput
    , loadReplicationWarmUpMeasures
    , loadReplicationWarmUpInput
    , loadReplicationInput
    , loadReplicationMeasures
    , loadResDataEndState
    , loadResDataStartState
    , deserialise
    , mDeserialise
    , fromRandGen
    , toRandGen
    , serialiseSeed
    , deserialiseSeed
    ) where


import           Control.Lens                         (over, view)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString                      as B
import           Data.Function                        (on)
import qualified Data.List                            as L
import           Data.Maybe                           (fromMaybe, isNothing, listToMaybe)
import           Data.Serialize                       as S (Serialize, get, put, runGet,
                                                            runPut)
import qualified Data.Text                            as T
import           Data.Time                            (getCurrentTime)
import           Data.Vector.Serialize                ()
import qualified Data.Vector.Unboxed                  as V
import           Data.Word                            (Word32)
import qualified Database.Esqueleto                   as E
import qualified Database.Esqueleto.Internal.Language as E
import           Database.Persist                     as P
import           Database.Persist.Postgresql          (SqlBackend)
import           System.Exit                          (exitFailure)
import           System.Random.MWC
import System.IO

import           Experimenter.Experiment
import           Experimenter.Input
import           Experimenter.Measure
import           Experimenter.Models
import           Experimenter.Parameter
import           Experimenter.Result.Type
import           Experimenter.Setup
import           Experimenter.StepResult
import           Experimenter.Util


import Debug.Trace


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
          liftIO $ hFlush stdout
          liftIO $ hFlush stderr
          liftIO exitFailure
          -- return Nothing
        Right r -> return $ Just r


loadExperimentResults :: (ExperimentDef a) => Key Exp -> ReaderT SqlBackend (LoggingT (ExpM a)) [ExperimentResult a]
loadExperimentResults kExp = do
  xs <- selectList [ExpResultExp ==. kExp] []
  mapM loadExperimentResult xs


loadResDataEndState :: forall backend resData a .
     (PersistUniqueRead backend, PersistQueryRead backend, BackendCompatible SqlBackend backend, PersistEntity resData, ExperimentDef a, PersistEntityBackend resData ~ BaseBackend backend)
  => Key Exp
  -> (resData -> Maybe ByteString)
  -> Key resData
  -> ReaderT backend (LoggingT (ExpM a)) (Maybe a)
loadResDataEndState expId acc k = do
  mResData <- P.get k
  res <- case mResData of
    Nothing  -> error "Could not get end state"
    Just resData -> do
      (mmSer :: Maybe (Maybe (Serializable a))) <- mDeserialise (T.pack "end state") (acc resData)
      lift $ lift $ maybe (return Nothing) (fmap Just . deserialisable) (join mmSer)
  traverse (setParams expId) res

loadResDataStartState ::
     (PersistUniqueRead backend, PersistQueryRead backend, BackendCompatible SqlBackend backend, PersistEntity resData, ExperimentDef a, PersistEntityBackend resData ~ BaseBackend backend)
  => Key Exp
  -> (resData -> ByteString)
  -> Key resData
  -> ReaderT backend (LoggingT (ExpM a)) a
loadResDataStartState expId acc k = do
  mResData <- P.get k
  res <- case mResData of
    Nothing -> error "Could not get start state"
    Just resData -> do
      ser <- fromMaybe (error "Could not deserialise preparation start state ") <$> deserialise "prep start state" (acc resData)
      lift $ lift $ deserialisable ser
  setParams expId res

setParams :: (PersistUniqueRead backend, PersistQueryRead backend, BackendCompatible SqlBackend backend, ExperimentDef a) => Key Exp -> a -> ReaderT backend (LoggingT (ExpM a)) a
setParams expId st = do
  paramSettings <- loadParamSetup expId
  foldM setParams' st paramSettings
  where setParams' st (ParameterSetting n bs drp _) =
           case L.find (\(ParameterSetup name _ _ _ _ _ _) -> name == n) parameterSetup of
             Nothing -> do
               $(logError) $ "Could not find parameter with name " <> n <> " in the current parameter setting. Thus it cannot be modified!"
               return st
             Just (ParameterSetup _ setter _ _ _ drp _) ->
               case runGet S.get bs of
                 Left err -> error $ "Could not read value of parameter " <> T.unpack n <> ". Aborting! Serializtion error was: " ++ err
                 Right val -> do
                   $(logInfo) $ "Loaded parameter '" <> n <> "' value: " <> tshow val
                   return $ setter val st
        parameterSetup = parameters st

loadExperimentResult :: forall a m . (ExperimentDef a) => Entity ExpResult -> ReaderT SqlBackend (LoggingT (ExpM a)) (ExperimentResult a)
loadExperimentResult (Entity k (ExpResult expId rep mPrepResDataId)) = do
  mEPrepResData <- fmap join $ sequence $ P.getEntity <$> mPrepResDataId
  prepRes <- case mEPrepResData of
    Nothing -> return Nothing
    Just (Entity resDataKey (PrepResultData startT endT startRandGenBS endRandGenBS startStBS endStBS startInpStBS endInpStBS)) -> do
      let startSt = AvailableOnDemand (loadResDataStartState expId (view prepResultDataStartState) resDataKey)
      let endSt = AvailableOnDemand (loadResDataEndState expId (view prepResultDataEndState) resDataKey)
      mStartInpSt <- deserialise "prep start input state" startInpStBS
      mEndInpSt <- mDeserialise "prep end input state" endInpStBS
      inpCount <- loadPreparationInputCount resDataKey
      resultCount <- loadPrepartionMeasuresCount resDataKey
      startRandGen <- toRandGen startRandGenBS
      endRandGen <- maybe (return Nothing) (fmap Just . toRandGen) endRandGenBS
      return $ do
        endInpSt <- mEndInpSt
        startInpSt <- mStartInpSt
        let inputVals = AvailableOnDemand (fromMaybe [] <$> loadPreparationInput resDataKey)
        let results = AvailableOnDemand (loadPrepartionMeasures resDataKey)
        return $ ResultData (ResultDataPrep resDataKey) startT endT startRandGen endRandGen (inpCount, inputVals) (resultCount, results) startSt endSt startInpSt endInpSt
  evalResults <- loadReplicationResults expId k
  return $ ExperimentResult k rep prepRes evalResults


serialiseSeed :: Seed -> ByteString
serialiseSeed seed = S.runPut $ S.put (fromSeed seed :: V.Vector Word32)

deserialiseSeed :: ByteString -> Seed
deserialiseSeed bs = toSeed (fromRight $ S.runGet S.get bs :: V.Vector Word32)
  where fromRight (Right s) = s
        fromRight (Left err) = error $ "Could not deserialise random generator. Error Message: " <> err

fromRandGen :: (MonadIO m) => GenIO -> m ByteString
fromRandGen ran = do
  vec <- liftIO (fromSeed <$> save ran)
  return $ S.runPut (S.put vec)


toRandGen :: (MonadIO m) => ByteString -> m GenIO
toRandGen bs = do
  case S.runGet S.get bs of
    Left err -> error $ "Could not deserialise random generator. Error Message: " <> err
    Right (vec :: V.Vector Word32) -> liftIO (restore $ toSeed vec)

-- loadParamSetup :: (PersistStoreRead backend, ExperimentDef a) => Key Exp -> ReaderT backend (LoggingT (ExpM a)) [ParameterSetting a]
-- loadParamSetup kExp = L.sortBy (compare `on` view parameterSettingName) . map (mkParameterSetting' . entityVal) <$> selectList [ParamSettingExp ==. kExp] []
loadParamSetup kExp = L.sortBy (compare `on` view parameterSettingName) . map (mkParameterSetting' . entityVal) <$> (E.select $ E.from $ \pm -> E.where_ (pm E.^. ParamSettingExp E.==. E.val kExp) >> return pm)
  where
    mkParameterSetting' (ParamSetting _ n v b design) = ParameterSetting n v b (toEnum design)

fromCount :: [E.Value Int] -> Int
fromCount = fromMaybe 0 . listToMaybe . fmap (\(E.Value v) -> v)

loadPreparationInputCount  :: (MonadIO m) => Key PrepResultData -> ReaderT SqlBackend m Int
loadPreparationInputCount kExpRes = count [PrepInputPrepResultData ==. kExpRes]

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
loadPrepartionMeasuresCount kExpRes = count [PrepMeasurePrepResultData ==. kExpRes]

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

loadReplicationResults :: (ExperimentDef a) => Key Exp -> Key ExpResult -> ReaderT SqlBackend (LoggingT (ExpM a)) [ReplicationResult a]
loadReplicationResults expId kExpRes = do
  xs <- selectList [RepResultExpResult ==. kExpRes] []
  L.sortBy (compare `on` view replicationNumber) <$> mapM (loadReplicationResult expId) xs


loadReplicationResult :: (ExperimentDef a) => Key Exp -> Entity RepResult -> ReaderT SqlBackend (LoggingT (ExpM a)) (ReplicationResult a)
loadReplicationResult expId (Entity k (RepResult _ repNr mWmUpResId mRepResId)) = do
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
      wmUpStartRandGen <- toRandGen (view warmUpResultDataStartRandGen wmUpRes)
      wmUpEndRandGen <- maybe (return Nothing) (fmap Just . toRandGen) (view warmUpResultDataEndRandGen wmUpRes)
      let wmUpStartSt = AvailableOnDemand (loadResDataStartState expId (view warmUpResultDataStartState) wmUpResKey)
      let wmUpEndSt = AvailableOnDemand (loadResDataEndState expId (view warmUpResultDataEndState) wmUpResKey)
      mWmUpStartInpSt <- deserialise "warm up start input state" (view warmUpResultDataStartInputState wmUpRes)
      mWmUpEndInpSt <- mDeserialise "warm up end input state" (view warmUpResultDataEndInputState wmUpRes)
      wmUpInpValsCount <- loadReplicationWarmUpInputCount wmUpResKey
      wmUpMeasuresCount <- loadReplicationWarmUpMeasuresCount wmUpResKey
      return $ do
        let wmUpInpVals = AvailableOnDemand (fromMaybe [] <$> loadReplicationWarmUpInput wmUpResKey)
            wmUpMeasures = AvailableOnDemand (loadReplicationWarmUpMeasures wmUpResKey)
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
      repStartRandGen <- toRandGen (view repResultDataStartRandGen repRes)
      repEndRandGen <- maybe (return Nothing) (fmap Just . toRandGen) (view repResultDataEndRandGen repRes)
      mRepStartInpSt <- deserialise "rep start input state" (view repResultDataStartInputState repRes)
      mRepEndInpSt <- mDeserialise "rep end input state" (view repResultDataEndInputState repRes)
      repInpValsCount <- loadReplicationInputCount repResKey
      repMeasuresCount <- loadReplicationMeasuresCount repResKey
      return $ do
        let repInpVals = AvailableOnDemand (fromMaybe [] <$> loadReplicationInput repResKey)
            repMeasures = AvailableOnDemand (loadReplicationMeasures repResKey)
        let repStartSt = AvailableOnDemand (loadResDataStartState expId (view repResultDataStartState) repResKey)
        let repEndSt = AvailableOnDemand (loadResDataEndState expId (view repResultDataEndState) repResKey)
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
loadReplicationWarmUpInputCount kExpRes = count [WarmUpInputRepResult ==. kExpRes]


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
loadReplicationWarmUpMeasuresCount kExpRes = count [WarmUpMeasureRepResult ==. kExpRes]

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
loadReplicationInputCount kExpRes = count [RepInputRepResult ==. kExpRes]

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
loadReplicationMeasuresCount kExpRes = count [RepMeasureRepResult ==. kExpRes]


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
          serSt <- lift $ lift $ sequence $ deserialisable <$> runGet S.get s
          let other = (,) <$> serSt <*> runGet S.get iS
          return $ fromEither False (equalExperiments (initSt, initInpSt) <$> other))
      expsList
  params <- mapM (\e -> selectList [ParamExps ==. entityKey e] [Asc ParamName]) exps
  let mkParamTpl (Param _ n minB maxB) = (n, minB, maxB)
  let myParams = L.sortBy (compare `on` (\(x, _, _) -> x)) $ map (mkParamTpl . convertParameterSetup (entityKey (head exps))) (parameters initSt)
  case L.find ((== myParams) . map (mkParamTpl . entityVal) . snd) (zip exps params) of
    Nothing -> do
      $(logInfo) "Starting new experiment..."
      time <-  liftIO getCurrentTime
      serInitSt <- lift $ lift $ serialisable initSt
      eExp <- insertEntity $ Exps name time Nothing (runPut $ put serInitSt) (runPut $ put initInpSt)
      void $ insert $ mkExpSetup eExp
      mapM_ (insertParam (entityKey eExp)) (parameters initSt)
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
