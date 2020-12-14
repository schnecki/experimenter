{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
module Experimenter.Result.Query
    ( loadExperiments
    , loadExperimentResults
    , loadExperimentsResults
    , loadPreparationInputWhere
    , loadPreparationMeasuresWhere
    , loadPreparationAggregateWhere
    , loadReplicationWarmUpInputWhere
    , loadReplicationWarmUpMeasuresWhere
    , loadReplicationWarmUpAggregateWhere
    , loadReplicationInputWhere
    , loadReplicationMeasuresWhere
    , loadReparationAggregateWhere
    , loadResDataEndState
    , loadResDataStartState
    , loadParamSetup
    , deserialise
    , mDeserialise
    , setParams
    , fromRandGen
    , toRandGen
    , serialiseSeed
    , deserialiseSeed
    , StartStateType (..)
    , EndStateType (..)
    , setResDataStartState
    , setResDataEndState
    ) where


import           Conduit                     as C
import           Control.DeepSeq             (force)
import           Control.Lens                (view)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B
import qualified Data.Conduit.List           as CL
import           Data.Either                 (isLeft)
import           Data.Function               (on)
import qualified Data.List                   as L
import           Data.Maybe                  (fromMaybe)
import           Data.Serialize              as S (Serialize, get, put, runGet, runPut)
import qualified Data.Text                   as T
import           Data.Time                   (getCurrentTime)
import           Data.Vector.Serialize       ()
import qualified Data.Vector.Unboxed         as V
import           Data.Word                   (Word32)
import qualified Database.Esqueleto          as E
import           Database.Persist            as P
import           Database.Persist.Postgresql (SqlBackend)
import           System.Random.MWC

import           Experimenter.Availability
import           Experimenter.DB
import           Experimenter.Experiment
import           Experimenter.Input
import           Experimenter.Measure
import           Experimenter.Models
import           Experimenter.Parameter
import           Experimenter.Result.Type
import           Experimenter.Setting
import           Experimenter.StepResult
import           Experimenter.Util


data EndStateType
  = EndStatePrep !(Key PrepResultData)
  | EndStateWarmUp !(Key WarmUpResultData)
  | EndStateRep !(Key RepResultData)


data StartStateType
  = StartStatePrep !(Key PrepResultData)
  | StartStateWarmUp !(Key WarmUpResultData)
  | StartStateRep !(Key RepResultData)


loadExperimentsResults :: (ExperimentDef a) => ExperimentSetting -> InputState a -> a -> Key Exps -> DB (ExpM a) (Maybe (Experiments a))
loadExperimentsResults setup initInpSt initSt key =
  runMaybeT $ do
    e <- MaybeT $ P.get key
    exps <- lift (L.sortBy (compare `on` view experimentNumber) <$> loadExperimentList key)
    expInfoParams <- lift (map entityVal <$> selectList [ExpsInfoParamExps ==. key] [])
    eSetup <- lift (fromMaybe (error "Setup not found. Your DB is corrupted!") <$> getBy (UniqueExpsSetup key))
    infoParams <- lift (mapM fromInfoParam expInfoParams)
    return $
      Experiments
        key
        (view expsName e)
        (view expsStartTime e)
        (view expsEndTime e)
        (entityVal eSetup)
        (parameters initSt)
        (concat infoParams)
        initSt
        initInpSt
        exps
  where
    infoParams = view experimentInfoParameters setup
    fromInfoParam (ExpsInfoParam _ n bs) =
      case L.find ((== n) . infoParameterName) infoParams of
        Nothing -> do
          $(logDebug) $ "Could not find parameter " <> n <> " in settings while loading values from the DB. It will not be reported therefore!"
          return []
        Just (ExperimentInfoParameter _ v) -> return [ExperimentInfoParameter n (fromEither v $ S.runGet S.get bs)]
    fromEither _ (Right x) = x
    fromEither d (Left _)  = d


loadExperiments :: (ExperimentDef a) => ExperimentSetting -> InputState a -> a -> DB (ExpM a) (Experiments a)
loadExperiments setup initInpSt initSt = do
  eExp <- getOrCreateExps setup initInpSt initSt
  let e = entityVal eExp
  exps <- L.sortBy (compare `on` view experimentNumber) <$> loadExperimentList (entityKey eExp)
  eSetup <- fromMaybe (error "Setup not found. Your DB is corrupted!") <$> getBy (UniqueExpsSetup (entityKey eExp))
  return $!
    Experiments
      (entityKey eExp)
      (view expsName e)
      (view expsStartTime e)
      (view expsEndTime e)
      (entityVal eSetup)
      (parameters initSt)
      (view experimentInfoParameters setup)
      initSt
      initInpSt
      exps


loadExperimentList :: (ExperimentDef a) => Key Exps -> DB (ExpM a) [Experiment a]
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
          -- $(logError) $ "Could not deserialise " <> n <> "! Discarding saved experiment result. Data length: " <> tshow (B.length bs) <> ". Error Message: " <> tshow err
          return Nothing
        Right r -> return $ Just r


loadExperimentResults :: (ExperimentDef a) => Key Exp -> DB (ExpM a) [ExperimentResult a]
loadExperimentResults kExp = do
  xs <- selectList [ExpResultExp ==. kExp] [Asc ExpResultRepetition]
  mapM loadExperimentResult xs


loadResDataEndState ::
     forall backend resData a. (ExperimentDef a)
  => Key Exp
  -> EndStateType
  -> DB (ExpM a) (Maybe a)
loadResDataEndState expId endState = do
  parts <-
    fmap force $
    case endState of
      EndStatePrep k -> fmap (view prepEndStatePartData . entityVal) <$> selectList [PrepEndStatePartResultData ==. k] [Asc PrepEndStatePartNumber]
      EndStateWarmUp k -> fmap (view warmUpEndStatePartData . entityVal) <$> selectList [WarmUpEndStatePartResultData ==. k] [Asc WarmUpEndStatePartNumber]
      EndStateRep k -> fmap (view repEndStatePartData . entityVal) <$> selectList [RepEndStatePartResultData ==. k] [Asc RepEndStatePartNumber]
  res <-
    if null parts
      then return Nothing
      else do
        mSer <- deserialise (T.pack "end state") (B.concat parts)
        lift $ lift $ lift $ maybe (return Nothing) (fmap Just . deserialisable) mSer
  force <$> traverse (setParams expId) res

loadResDataStartState :: (ExperimentDef a) => Key Exp -> StartStateType -> DB (ExpM a) a
loadResDataStartState expId startState = do
  parts <-
    fmap force $
    case startState of
      StartStatePrep k -> fmap (view prepStartStatePartData . entityVal) <$> selectList [PrepStartStatePartResultData ==. k] [Asc PrepStartStatePartNumber]
      StartStateWarmUp k -> fmap (view warmUpStartStatePartData . entityVal) <$> selectList [WarmUpStartStatePartResultData ==. k] [Asc WarmUpStartStatePartNumber]
      StartStateRep k -> fmap (view repStartStatePartData . entityVal) <$> selectList [RepStartStatePartResultData ==. k] [Asc RepStartStatePartNumber]
  res <-
    if null parts
      then error "Could not get start state"
      else do
        ser <- fromMaybe (error "Could not deserialise start state ") <$> deserialise "prep start state" (B.concat parts)
        lift $ lift $ lift $ deserialisable ser
  force <$> setParams expId res


setResDataEndState :: (MonadIO m) => EndStateType -> Maybe ByteString -> ReaderT SqlBackend m ()
setResDataEndState (EndStatePrep k) (Just bs) = do
  liftIO $ B.writeFile "/tmp/EndState" bs
  let parts = splitState bs
  mapM_ (\(nr, part) -> upsert (PrepEndStatePart k nr part) [PrepEndStatePartData =. part]) (zip [0..] parts)
  deleteWhere [PrepEndStatePartResultData ==. k, PrepEndStatePartNumber >=. length parts]
setResDataEndState (EndStateWarmUp k) (Just bs) = do
  let parts = splitState bs
  mapM_ (\(nr, part) -> upsert (WarmUpEndStatePart k nr part) [WarmUpEndStatePartData =. part]) (zip [0..] parts)
  deleteWhere [WarmUpEndStatePartResultData ==. k, WarmUpEndStatePartNumber >=. length parts]
setResDataEndState (EndStateRep k) (Just bs) = do
  let parts = splitState bs
  mapM_ (\(nr, part) -> upsert (RepEndStatePart k nr part) [RepEndStatePartData =. part]) (zip [0..] parts)
  deleteWhere [RepEndStatePartResultData ==. k, RepEndStatePartNumber >=. length parts]
setResDataEndState (EndStatePrep k) Nothing = deleteWhere [PrepEndStatePartResultData ==. k]
setResDataEndState (EndStateWarmUp k) Nothing = deleteWhere [WarmUpEndStatePartResultData ==. k]
setResDataEndState (EndStateRep k) Nothing = deleteWhere [RepEndStatePartResultData ==. k]


setResDataStartState :: (MonadIO m) => StartStateType -> ByteString -> ReaderT SqlBackend m ()
setResDataStartState (StartStatePrep k) bs = do
  let parts = splitState bs
  mapM_ (\(nr, part) -> upsert (PrepStartStatePart k nr part) [PrepStartStatePartData =. part]) (zip [0..] parts)
  deleteWhere [PrepStartStatePartResultData ==. k, PrepStartStatePartNumber >=. length parts]
setResDataStartState (StartStateWarmUp k) bs = do
  let parts = splitState bs
  mapM_ (\(nr, part) -> upsert (WarmUpStartStatePart k nr part) [WarmUpStartStatePartData =. part]) (zip [0..] parts)
  deleteWhere [WarmUpStartStatePartResultData ==. k, WarmUpStartStatePartNumber >=. length parts]
setResDataStartState (StartStateRep k) bs = do
  let parts = splitState bs
  mapM_ (\(nr, part) -> upsert (RepStartStatePart k nr part) [RepStartStatePartData =. part]) (zip [0..] parts)
  deleteWhere [RepStartStatePartResultData ==. k, RepStartStatePartNumber >=. length parts]


splitState :: ByteString -> [ByteString]
splitState bs
  | B.null bs = []
  | otherwise = B.take splitLength bs : splitState (B.drop splitLength bs)
  where
    splitLength = 128000000     -- 128MB as ByteString is composed of Word8 (8-bit unsigned integer = 1 byte) elements


setParams :: (MonadIO m, ExperimentDef a) => Key Exp -> a -> ReaderT SqlBackend (LoggingT m) a
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

loadExperimentResult :: forall a . (ExperimentDef a) => Entity ExpResult -> DB (ExpM a) (ExperimentResult a)
loadExperimentResult (Entity k (ExpResult expId rep mPrepResDataId)) = do
  mEPrepResData <- fmap join $ sequence $ P.getEntity <$> mPrepResDataId
  prepRes <-
    case mEPrepResData of
      Nothing -> return Nothing
      Just (Entity resDataKey (PrepResultData startT endT startRandGenBS endRandGenBS startInpStBS endInpStBS)) -> do
        let startSt = AvailableOnDemand (loadResDataStartState expId (StartStatePrep resDataKey))
        let endSt = AvailableOnDemand (loadResDataEndState expId (EndStatePrep resDataKey))
        mStartInpSt <- deserialise "prep start input state" startInpStBS
        mEndInpSt <- mDeserialise "prep end input state" endInpStBS
        inpCount <- loadPreparationInputCount resDataKey
        resultCount <- loadPrepartionMeasuresCount resDataKey
        startRandGen <- toRandGen startRandGenBS
        endRandGen <- maybe (return Nothing) (fmap Just . toRandGen) endRandGenBS
        let inputVals = AvailableListOnDemand (inpCount, loadPreparationInputWhere resDataKey)
        let results = AvailableListOnDemand (resultCount, loadPreparationMeasuresWhere resDataKey)
        return $ ResultData (ResultDataPrep resDataKey) startT endT startRandGen endRandGen inputVals results startSt endSt <$> mStartInpSt <*> mEndInpSt
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
toRandGen bs =
  case S.runGet S.get bs of
    Left err -> error $ "Could not deserialise random generator. Error Message: " <> err
    Right (vec :: V.Vector Word32) -> liftIO (restore $ toSeed vec)


loadParamSetup :: (MonadIO m) => Key Exp -> ReaderT SqlBackend m [ParameterSetting a1]
loadParamSetup kExp =
  L.sortBy (compare `on` view parameterSettingName) . map (mkParameterSetting' . entityVal) <$>
  E.select (E.from $ \pm -> E.where_ (pm E.^. ParamSettingExp E.==. E.val kExp) >> return pm)
  where
    mkParameterSetting' (ParamSetting _ n v b design) = ParameterSetting n v b (toEnum design)

-- fromCount :: [E.Value Int] -> Int
-- fromCount = fromMaybe 0 . listToMaybe . fmap (\(E.Value v) -> v)

loadPreparationInputCount  :: (MonadIO m) => Key PrepResultData -> ReaderT SqlBackend m Int
loadPreparationInputCount kExpRes = count [PrepInputPrepResultData ==. kExpRes]

loadPreparationInputWhere :: (MonadIO m, ExperimentDef a) => Key PrepResultData -> AvailabilityListWhere -> ConduitM () (Input a) (DB m) ()
loadPreparationInputWhere kExpRes (PrepInputWhere where') = do
  let src =
        E.selectSource $
        E.from $ \(prepI, prepIV) -> do
          E.where_ (prepI E.^. PrepInputId E.==. prepIV E.^. PrepInputValuePrepInput)
          E.where_ (prepI E.^. PrepInputPrepResultData E.==. E.val kExpRes)
          where' prepI prepIV
          return (prepI, prepIV)
  src C..| C.mapMC mkInput C..| sequenceC []
  -- sequence <$> mapM mkInput res
  where
    mkInput :: (MonadIO m, MonadLogger m, ExperimentDef a) => (Entity PrepInput, Entity PrepInputValue) -> m (Maybe (Input a))
    mkInput (Entity _ (PrepInput _ p), Entity _ (PrepInputValue _ v)) = do
      v' <- deserialise "prep input value" v
      return $ Input p <$> v'
loadPreparationInputWhere kExpRes GetAll = loadPreparationInputWhere kExpRes (PrepInputWhere (\_ _ -> return ()))
loadPreparationInputWhere _ where' = error $ "Wrong Where clause: " ++ show where' ++ " where PrepInputWhere was expected"


sequenceC :: Monad m => [Maybe o] -> ConduitT (Maybe o) o m ()
sequenceC xs = do
      mx <- await
      case mx of
        Nothing -> CL.sourceList (fromMaybe [] $ sequence $ reverse xs)
        Just x  -> sequenceC (x:xs)


loadPrepartionMeasuresCount :: (MonadIO m) => Key PrepResultData -> DB m Int
loadPrepartionMeasuresCount kExpRes = count [PrepMeasurePrepResultData ==. kExpRes]

-- loadPrepartionMeasures :: (MonadIO m) => Key PrepResultData -> ConduitT () Measure (DB m) ()
-- loadPrepartionMeasures = loadPrepartionMeasuresWith (\_ _ -> return ())

loadPreparationMeasuresWhere :: (MonadIO m) => Key PrepResultData -> AvailabilityListWhere -> ConduitT () Measure (DB m) ()
loadPreparationMeasuresWhere kExpRes (PrepMeasureWhere where') = do
  let src =
        E.selectSource $
        E.from $ \(prepM, prepRS) -> do
          E.where_ (prepM E.^. PrepMeasureId E.==. prepRS E.^. PrepResultStepMeasure)
          E.where_ (prepM E.^. PrepMeasurePrepResultData E.==. E.val kExpRes)
          where' prepM prepRS
          E.orderBy [E.asc (prepM E.^. PrepMeasurePeriod)]
          return (prepM, prepRS)
  src C..| C.mapC mkMeasure C..| CL.groupBy ((==) `on` view measurePeriod) C..| C.mapC combineMeasures
  -- return $ map combineMeasures $ L.groupBy ((==) `on` view measurePeriod) $ map mkMeasure res
  where
    mkMeasure (Entity _ (PrepMeasure _ p), Entity _ (PrepResultStep _ n x y)) = Measure p [StepResult n x y]
    combineMeasures xs@(Measure p _:_) = Measure p (concatMap (view measureResults) xs)
    combineMeasures _                  = error "not possible"
loadPreparationMeasuresWhere kExpRes GetAll = loadPreparationMeasuresWhere kExpRes (PrepMeasureWhere (\_ _ -> return ()))
loadPreparationMeasuresWhere _ where' = error $ "Wrong Where clause: " ++ show where' ++ " where PrepMeasuresWhere was expected"


loadPreparationAggregateWhere :: (MonadIO m) => Key PrepResultData -> AggregateFunction -> AvailabilityListWhere -> DB m Double
loadPreparationAggregateWhere kExpRes agg (PrepMeasureWhere where') =
  fmap (fromMaybe 0 . E.unValue . head) $
  E.select $
  E.from $ \(prepM, prepRS) -> do
    E.where_ (prepM E.^. PrepMeasureId E.==. prepRS E.^. PrepResultStepMeasure)
    E.where_ (prepM E.^. PrepMeasurePrepResultData E.==. E.val kExpRes)
    where' prepM prepRS
    return (agg $ prepRS E.^. PrepResultStepYValue)
loadPreparationAggregateWhere kExpRes agg GetAll = loadPreparationAggregateWhere kExpRes agg (PrepMeasureWhere (\_ _ -> return ()))
loadPreparationAggregateWhere _ _ where' = error $ "Wrong Where clause: " ++ show where' ++ " where loadPreparationAggregateWhere was expected"


loadReplicationResults :: (ExperimentDef a) => Key Exp -> Key ExpResult -> DB (ExpM a) [ReplicationResult a]
loadReplicationResults expId kExpRes = do
  xs <- selectList [RepResultExpResult ==. kExpRes] [Asc RepResultRepNr]
  mapM (loadReplicationResult expId) xs


loadReplicationResult :: (ExperimentDef a) => Key Exp -> Entity RepResult -> DB (ExpM a) (ReplicationResult a)
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
      let wmUpStartSt = AvailableOnDemand (loadResDataStartState expId (StartStateWarmUp wmUpResKey))
      let wmUpEndSt = AvailableOnDemand (loadResDataEndState expId (EndStateWarmUp wmUpResKey))
      mWmUpStartInpSt <- deserialise "warm up start input state" (view warmUpResultDataStartInputState wmUpRes)
      mWmUpEndInpSt <- mDeserialise "warm up end input state" (view warmUpResultDataEndInputState wmUpRes)
      wmUpInpValsCount <- loadReplicationWarmUpInputCount wmUpResKey
      wmUpMeasuresCount <- loadReplicationWarmUpMeasuresCount wmUpResKey
      let wmUpInpVals = AvailableListOnDemand (wmUpInpValsCount, loadReplicationWarmUpInputWhere wmUpResKey)
          wmUpMeasures = AvailableListOnDemand (wmUpMeasuresCount, loadReplicationWarmUpMeasuresWhere wmUpResKey)
      return $
        ResultData (ResultDataWarmUp wmUpResKey) wmUpStartTime wmUpEndTime wmUpStartRandGen wmUpEndRandGen wmUpInpVals wmUpMeasures wmUpStartSt wmUpEndSt <$> mWmUpStartInpSt <*>
        mWmUpEndInpSt
    mkRep (Entity repResKey repRes) = do
      let repStartTime = view repResultDataStartTime repRes
      let repEndTime = view repResultDataEndTime repRes
      repStartRandGen <- toRandGen (view repResultDataStartRandGen repRes)
      repEndRandGen <- maybe (return Nothing) (fmap Just . toRandGen) (view repResultDataEndRandGen repRes)
      mRepStartInpSt <- deserialise "rep start input state" (view repResultDataStartInputState repRes)
      mRepEndInpSt <- mDeserialise "rep end input state" (view repResultDataEndInputState repRes)
      repInpValsCount <- loadReplicationInputCount repResKey
      repMeasuresCount <- loadReplicationMeasuresCount repResKey
      let repInpVals = AvailableListOnDemand (repInpValsCount, loadReplicationInputWhere repResKey)
          repMeasures = AvailableListOnDemand (repMeasuresCount, loadReplicationMeasuresWhere repResKey)
      let repStartSt = AvailableOnDemand (loadResDataStartState expId (StartStateRep repResKey))
      let repEndSt = AvailableOnDemand (loadResDataEndState expId (EndStateRep repResKey))
      return $
        ResultData (ResultDataRep repResKey) repStartTime repEndTime repStartRandGen repEndRandGen repInpVals repMeasures repStartSt repEndSt <$> mRepStartInpSt <*> mRepEndInpSt


loadReplicationWarmUpInputCount :: (MonadIO m) => Key WarmUpResultData -> ReaderT SqlBackend m Int
loadReplicationWarmUpInputCount kExpRes = count [WarmUpInputRepResult ==. kExpRes]


loadReplicationWarmUpInputWhere :: (ExperimentDef a, MonadIO m) => Key WarmUpResultData -> AvailabilityListWhere -> ConduitT () (Input a) (DB m) ()
loadReplicationWarmUpInputWhere kExpRes (WarmUpInputWhere where') = do
  let src =
       E.selectSource $
       E.from $ \(warmUpI, warmUpIV) -> do
         E.where_ (warmUpI E.^. WarmUpInputId E.==. warmUpIV E.^. WarmUpInputValueWarmUpInput)
         E.where_ (warmUpI E.^. WarmUpInputRepResult E.==. E.val kExpRes)
         where' warmUpI warmUpIV
         return (warmUpI, warmUpIV)
  src C..| C.mapMC mkInput C..| sequenceC []
  -- sequence <$> mapM mkInput res
  where
    mkInput (Entity _ (WarmUpInput _ p), Entity _ (WarmUpInputValue _ v)) = do
      v' <- deserialise "warm up input value" v
      return $ Input p <$> v'
loadReplicationWarmUpInputWhere kExpRes GetAll = loadReplicationWarmUpInputWhere kExpRes (WarmUpInputWhere (\_ _ -> return ()))
loadReplicationWarmUpInputWhere _ where' = error $ "Wrong Where clause: " ++ show where' ++ " where PrepInputWhere was expected"


loadReplicationWarmUpMeasuresCount :: (MonadIO m) => Key WarmUpResultData -> ReaderT SqlBackend m Int
loadReplicationWarmUpMeasuresCount kExpRes = count [WarmUpMeasureRepResult ==. kExpRes]

loadReplicationWarmUpMeasuresWhere :: (MonadIO m) => Key WarmUpResultData -> AvailabilityListWhere -> ConduitM () Measure (DB m) ()
loadReplicationWarmUpMeasuresWhere kExpRes (WarmUpMeasureWhere where') = do
  let src =
        E.selectSource $
        E.from $ \(warmUpM, warmUpRS) -> do
          E.where_ (warmUpM E.^. WarmUpMeasureId E.==. warmUpRS E.^. WarmUpResultStepMeasure)
          E.where_ (warmUpM E.^. WarmUpMeasureRepResult E.==. E.val kExpRes)
          where' warmUpM warmUpRS
          E.orderBy [E.asc (warmUpM E.^. WarmUpMeasurePeriod)]
          return (warmUpM, warmUpRS)
  src C..| C.mapC mkMeasure C..| CL.groupBy ((==) `on` view measurePeriod) C..| C.mapC combineMeasures
  where
    mkMeasure (Entity _ (WarmUpMeasure _ p), Entity _ (WarmUpResultStep _ n x y)) = Measure p [StepResult n x y]
    combineMeasures xs@(Measure p _:_) = Measure p (concatMap (view measureResults) xs)
    combineMeasures _                  = error "not possible"
loadReplicationWarmUpMeasuresWhere kExpRes GetAll = loadReplicationWarmUpMeasuresWhere kExpRes (WarmUpMeasureWhere (\_ _ -> return ()))
loadReplicationWarmUpMeasuresWhere _ where' = error $ "Wrong Where clause: " ++ show where' ++ " where PrepMeasuresWhere was expected"

loadReplicationWarmUpAggregateWhere :: (MonadIO m) => Key WarmUpResultData -> AggregateFunction -> AvailabilityListWhere -> DB m Double
loadReplicationWarmUpAggregateWhere kExpRes agg (WarmUpMeasureWhere where') =
  fmap (fromMaybe 0 . E.unValue . head) $
  E.select $
  E.from $ \(warmUpM, warmUpRS) -> do
    E.where_ (warmUpM E.^. WarmUpMeasureId E.==. warmUpRS E.^. WarmUpResultStepMeasure)
    E.where_ (warmUpM E.^. WarmUpMeasureRepResult E.==. E.val kExpRes)
    where' warmUpM warmUpRS
    return (agg $ warmUpRS E.^. WarmUpResultStepYValue)
loadReplicationWarmUpAggregateWhere kExpRes agg GetAll = loadReplicationWarmUpAggregateWhere kExpRes agg (WarmUpMeasureWhere (\_ _ -> return ()))
loadReplicationWarmUpAggregateWhere _ _ where' = error $ "Wrong Where clause: " ++ show where' ++ " where loadReplicationWarmUpAggregateWhere was expected"


loadReplicationInputCount :: (MonadIO m) => Key RepResultData -> ReaderT SqlBackend m Int
loadReplicationInputCount kExpRes = count [RepInputRepResult ==. kExpRes]

loadReplicationInputWhere :: (ExperimentDef a, MonadIO m) => Key RepResultData -> AvailabilityListWhere -> ConduitT () (Input a) (DB m) ()
loadReplicationInputWhere kExpRes (RepInputWhere where') = do
  let src =
       E.selectSource $
       E.from $ \(repI, repIV) -> do
         E.where_ (repI E.^. RepInputId E.==. repIV E.^. RepInputValueRepInput)
         E.where_ (repI E.^. RepInputRepResult E.==. E.val kExpRes)
         where' repI repIV
         return (repI, repIV)
  src C..| C.mapMC mkInput C..| sequenceC []
  -- sequence <$> mapM mkInput res
  where
    mkInput (Entity _ (RepInput _ p), Entity _ (RepInputValue _ v)) = do
      v' <- deserialise "eval input value" v
      return $ Input p <$> v'
loadReplicationInputWhere kExpRes GetAll = loadReplicationInputWhere kExpRes (RepInputWhere (\_ _ -> return ()))
loadReplicationInputWhere _ where' = error $ "Wrong Where clause: " ++ show where' ++ " where RepInputWhere was expected"


loadReplicationMeasuresCount :: (MonadIO m) => Key RepResultData -> ReaderT SqlBackend m Int
loadReplicationMeasuresCount kExpRes = count [RepMeasureRepResult ==. kExpRes]


loadReplicationMeasuresWhere :: (MonadIO m) => Key RepResultData -> AvailabilityListWhere -> ConduitT () Measure (DB m) ()
loadReplicationMeasuresWhere kExpRes (RepMeasureWhere where') = do
  let src =
        E.selectSource $
        E.from $ \(repM, repRS) -> do
          E.where_ (repM E.^. RepMeasureId E.==. repRS E.^. RepResultStepMeasure)
          E.where_ (repM E.^. RepMeasureRepResult E.==. E.val kExpRes)
          where' repM repRS
          E.orderBy [E.asc (repM E.^. RepMeasurePeriod)]
          return (repM, repRS)
  src C..| C.mapC mkMeasure C..| CL.groupBy ((==) `on` view measurePeriod) C..| C.mapC combineMeasures
  where
    mkMeasure (Entity _ (RepMeasure _ p), Entity _ (RepResultStep _ n x y)) = Measure p [StepResult n x y]
    combineMeasures xs@(Measure p _:_) = Measure p (concatMap (view measureResults) xs)
    combineMeasures _                  = error "not possible"
loadReplicationMeasuresWhere kExpRes GetAll = loadReplicationMeasuresWhere kExpRes (RepMeasureWhere (\_ _ -> return ()))
loadReplicationMeasuresWhere _ where' = error $ "Wrong Where clause: " ++ show where' ++ " where RepMeasuresWhere was expected"


loadReparationAggregateWhere :: (MonadIO m) => Key RepResultData -> AggregateFunction -> AvailabilityListWhere -> DB m Double
loadReparationAggregateWhere kExpRes agg (RepMeasureWhere where') =
  fmap (fromMaybe 0 . E.unValue . head) $
  E.select $
  E.from $ \(repM, repRS) -> do
    E.where_ (repM E.^. RepMeasureId E.==. repRS E.^. RepResultStepMeasure)
    E.where_ (repM E.^. RepMeasureRepResult E.==. E.val kExpRes)
    where' repM repRS
    return (agg $ repRS E.^. RepResultStepYValue)
loadReparationAggregateWhere kExpRes agg GetAll = loadReparationAggregateWhere kExpRes agg (RepMeasureWhere (\_ _ -> return ()))
loadReparationAggregateWhere _ _ where' = error $ "Wrong Where clause: " ++ show where' ++ " where RepAggregateWhere was expected"


getOrCreateExps :: forall a . (ExperimentDef a) => ExperimentSetting -> InputState a -> a -> DB (ExpM a) (Entity Exps)
getOrCreateExps setup initInpSt initSt = do
  let name = view experimentBaseName setup
  expsList <- selectList [ExpsName ==. name] []
  expsInfoParams <- map (map entityVal) <$> mapM (\(Entity e _) -> selectList [ExpsInfoParamExps ==. e] []) expsList
  let expsList' = map fst $ filter ((\xs -> length infoParams >= length xs && all matchesExpsInfoParam xs) . snd) (zip expsList expsInfoParams)
  when (null expsList') $ $(logInfo) "No experiment with same Experiment Info Parameters found!"
  exps <-
    filterM
      (\(Entity _ (Exps _ _ _ s iS)) -> do
         serSt <- lift $ lift $ lift $ sequence $ deserialisable <$> runGet S.get s
         let other = (,) <$> serSt <*> runGet S.get iS
         when (isLeft other) $ $(logInfo) $ "Could not deserialise experiment with same name"
         return $ fromEither False (equalExperiments (initSt, initInpSt) <$> other))
      expsList'
  when (not (null expsList') && null exps) $ $(logInfo) "Found experiments with same name, but the are different or not deserialisable!"
  params <- mapM (\e -> selectList [ParamExps ==. entityKey e] [Asc ParamName]) exps
  let mkParamTpl (Param _ n _ _) = n
  let ~myParams = L.sort $ map (mkParamTpl . convertParameterSetup (entityKey (head exps))) (parameters initSt)
  case L.find ((== myParams) . L.sort . map (mkParamTpl . entityVal) . snd) (zip exps params) of
    Nothing -> do
      $(logInfo) "Starting new experiment..."
      time <- liftIO getCurrentTime
      !serInitSt <- lift $ lift $ lift $ serialisable initSt
      eExp <- insertEntity $ Exps name time Nothing (runPut $ put serInitSt) (runPut $ put initInpSt)
      void $ insert $ mkExpSetup eExp
      mapM_ (insertInfoParam (entityKey eExp)) infoParams
      mapM_ (insertParam (entityKey eExp)) (parameters initSt)
      return eExp
    Just (eExp, _) -> do
      $(logInfo) "Found experiment with same name and parameter settings. Continuing experiment ..."
      expInfoParams <- map entityVal <$> selectList [ExpsInfoParamExps ==. entityKey eExp] []
      mapM_ (insertInfoParam (entityKey eExp)) (filter ((`notElem` map (view expsInfoParamName) expInfoParams) . infoParameterName) infoParams)
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
        (max 1 <$> view evaluationMaxStepsBetweenSaves setup)
    insertInfoParam k (ExperimentInfoParameter n v) = insert $ ExpsInfoParam k n (S.runPut $ S.put v)
    insertParam :: Key Exps -> ParameterSetup a -> DB (ExpM a) (Key Param)
    insertParam eExp (ParameterSetup n _ _ _ (Just (minVal, maxVal)) _ _) = insert $ Param eExp n (Just $ runPut $ put minVal) (Just $ runPut $ put maxVal)
    insertParam eExp (ParameterSetup n _ _ _ Nothing _ _) = insert $ Param eExp n Nothing Nothing
    infoParams = view experimentInfoParameters setup
    matchesExpsInfoParam (ExpsInfoParam _ n bs) =
      case L.find ((== n) . infoParameterName) infoParams of
        Nothing -> False
        Just (ExperimentInfoParameter _ p) -> fromEither False ((p ==) <$> S.runGet S.get bs)
