{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Experimenter.Run
    ( DatabaseSetup (..)
    , execExperiments
    , runExperiments
    , runExperimentsIO
    ) where

import           Control.Arrow                (first, (&&&), (***))
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad                (forM)
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger         (LogLevel (..), LoggingT, MonadLogger,
                                               WriterLoggingT, defaultLoc, filterLogger,
                                               logDebug, logError, logInfo,
                                               runFileLoggingT, runLoggingT, runNoLoggingT,
                                               runStderrLoggingT, runStdoutLoggingT)
import           Data.List                    (foldl')

import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as BS
import           Data.Function                (on)
import qualified Data.List                    as L
import           Data.Maybe                   (fromJust, fromMaybe, isJust, isNothing)
import           Data.Pool                    as P
import           Data.Serialize               hiding (get)
import qualified Data.Serialize               as S
import qualified Data.Text                    as T
import           Data.Time                    (diffUTCTime, getCurrentTime)
import qualified Database.Esqueleto           as E
import           Database.Persist.Postgresql
import           Network.HostName             (HostName, getHostName)
import           System.IO
import           System.Posix.Process
import           System.Posix.Signals
import           System.Random


import           Experimenter.DatabaseSetup
import           Experimenter.Experiment
import           Experimenter.Input
import           Experimenter.Measure
import           Experimenter.Models
import           Experimenter.Parameter
import           Experimenter.Result
import           Experimenter.Setup
import           Experimenter.StepResult
import           Experimenter.Util

import           Debug.Trace


type Updated = Bool


execExperiments :: (ExperimentDef a) => (ExpM a (Bool, Experiments a) -> IO (Bool, Experiments a)) -> DatabaseSetup -> ExperimentSetup -> InputState a -> a -> IO (Experiments a)
execExperiments runExpM dbSetup setup initInpSt initSt = snd <$> runExperiments runExpM dbSetup setup initInpSt initSt

runExperiments :: (ExperimentDef a) => (ExpM a (Bool, Experiments a) -> IO (Bool, Experiments a)) -> DatabaseSetup -> ExperimentSetup -> InputState a -> a -> IO (Bool, Experiments a)
runExperiments = runner

runExperimentsIO :: (ExperimentDef a, IO ~ ExpM a) => DatabaseSetup -> ExperimentSetup -> InputState a -> a -> IO (Bool, Experiments a)
runExperimentsIO = runner id


runner :: (ExperimentDef a) => (ExpM a (Bool, Experiments a) -> IO (Bool, Experiments a)) -> DatabaseSetup -> ExperimentSetup -> InputState a -> a -> IO (Bool, Experiments a)
runner runExpM dbSetup setup initInpSt initSt = do
  runStdoutLoggingT $ withPostgresqlPool (connectionString dbSetup) (parallelConnections dbSetup) $ liftSqlPersistMPool $ runMigration migrateAll
  runExpM $
    (runStdoutLoggingT . filterLogger (\s _ -> s /= "SQL")) $
    withPostgresqlConn (connectionString dbSetup) $ \backend -> flip runSqlConn backend $ loadExperiments setup initInpSt initSt >>= checkUniqueParamNames >>= runExperimenter

timeout :: Num t => t
timeout = 10

runExperimenter :: (ExperimentDef a) => Experiments a -> ReaderT SqlBackend (LoggingT (ExpM a)) (Bool, Experiments a)
runExperimenter exps = do
  pid <- liftIO getProcessID
  hostName <- liftIO getHostName
  time <- liftIO getCurrentTime
  maybeMaster <- insertUnique $ ExpsMaster (exps ^. experimentsKey) (T.pack hostName) (fromIntegral pid) time
  transactionSave
  case maybeMaster of
    Just master -- installHandler sigKill deleteMaster Nothing >>
     -> runExperiment exps
    Nothing -> do
      mMaster <- getBy $ UniqueExpsMaster (exps ^. experimentsKey)
      case mMaster of
        Nothing -> runExperimenter exps
        Just (Entity masterId master) ->
          if diffUTCTime time (master ^. expsMasterLastAliveSign) > 2 * timeout
            then delete masterId >> runExperiment exps
            else undefined
      -- aquireExperimentLock >>= runExperimentAsClient


runExperiment :: (ExperimentDef a) => Experiments a -> ReaderT SqlBackend (LoggingT (ExpM a)) (Bool, Experiments a)
runExperiment exps = do
  (anyChange, exps') <- continueExperiments exps
  if anyChange
    then first (const True) <$> runExperiment exps'
    else return (anyChange, exps')

checkUniqueParamNames :: (Monad m) => Experiments a -> m (Experiments a)
checkUniqueParamNames exps = do
  let paramNames = map parameterName (view experimentsParameters exps)
  when (any ((> 1) . length) (L.group $ L.sort paramNames)) $ error "Parameter names must be unique!"
  return exps


continueExperiments :: (ExperimentDef a) => Experiments a -> ReaderT SqlBackend (LoggingT (ExpM a)) (Bool, Experiments a)
continueExperiments exp = do
  $(logInfo) $ "Processing experiment with ID " <> tshow (unSqlBackendKey $ unExpsKey $ exp ^. experimentsKey)
  liftIO $ hFlush stdout
  let exps = exp ^. experiments
  rands <- liftIO $ mkRands exps
  let params = parameters (exp ^. experimentsInitialState)
  newExps <- mkNewExps exp exps
  let expsList = exps ++ newExps
  $(logInfo) $ "Number of experiments loaded: " <> tshow (length exps)
  $(logInfo) $ "Number of new experiments: " <> tshow (length newExps)
  expRes <- mapM (continueExperiment rands exp) expsList
  let updated = any fst expRes
      res = map snd expRes
  if updated
    then do
      endTime <- return <$> liftIO getCurrentTime
      update (exp ^. experimentsKey) [ExpsEndTime =. endTime]
      return (updated, set experiments res $ set experimentsEndTime endTime exp)
    else return (updated, set experiments res exp)
  where
    mkRands :: [Experiment a] -> IO ([StdGen], [StdGen], [StdGen])
    mkRands [] = do
      prep <- replicateM repetits newStdGen
      wmUp <- replicateM (repetits * replicats) newStdGen
      repl <- replicateM (repetits * replicats) newStdGen
      return (prep, wmUp, repl)
    mkRands (x:_) = do
      let currentPrep = x ^.. experimentResults . traversed . preparationResults . traversed . startRandGen
          currentWmUp = x ^.. experimentResults . traversed . evaluationResults . traversed . warmUpResults . traversed . startRandGen
          currentRepl = x ^.. experimentResults . traversed . evaluationResults . traversed . evalResults . traversed . startRandGen
      prepNew <- replicateM (repetits - length currentPrep) newStdGen
      wmUpNew <- replicateM (repetits * replicats - length currentWmUp) newStdGen
      replNew <- replicateM (repetits * replicats - length currentRepl) newStdGen
      return (currentPrep ++ prepNew, currentWmUp ++ wmUpNew, currentRepl ++ replNew)
    repetits = exp ^. experimentsSetup . expsSetupRepetitions
    replicats = exp ^. experimentsSetup . expsSetupEvaluationReplications

saveParamSettings :: (MonadIO m) =>Key Exp -> [ParameterSetting a] -> ReaderT SqlBackend m ()
saveParamSettings kExp = mapM_ (\(ParameterSetting n bs drp design) -> insert $ ParamSetting kExp n bs drp (fromEnum design))


initParams :: Experiments a -> [ParameterSetting a]
initParams exp = map (mkParamSetting exp) (view experimentsParameters exp)
  where
    mkParamSetting :: Experiments a -> ParameterSetup a -> ParameterSetting a
    mkParamSetting exp (ParameterSetup name setter getter mod bnds drp design) =
      let v = getter (exp ^. experimentsInitialState)
      in ParameterSetting name (runPut $ put $ getter (exp ^. experimentsInitialState)) (maybe False (\x -> x v) drp) (maybe FullFactory (\x -> x v) design)


mkNoParamExp :: (ExperimentDef a) => Experiments a -> ReaderT SqlBackend (LoggingT (ExpM a)) [Experiment a]
mkNoParamExp exp = do
  $(logInfo) $ "Initializing new experiment without any parameters..."
  existing <- selectList [ExpExps ==. (exp ^. experimentsKey)] []
  if not (null existing)
    then return []
    else do
      startTime <- liftIO getCurrentTime
      kExp <- insert $ Exp (exp ^. experimentsKey) 1 startTime Nothing
      saveParamSettings kExp (initParams exp)
      return [Experiment kExp 1 startTime Nothing (initParams exp) []]


mkNewExps :: (ExperimentDef a) => Experiments a -> [Experiment a] -> ReaderT SqlBackend (LoggingT (ExpM a)) [Experiment a]
mkNewExps exp expsDone = do
  $(logInfo) "Checking whether adding further experiments is necessary..."
  -- params <- liftIO $ shuffleM $ parameters (exp ^. experimentsInitialState)
  let params = parameters (exp ^. experimentsInitialState)
  if null params
    then mkNoParamExp exp
    else do
      $(logInfo) "Creating new experiment variants"
      startTime <- liftIO getCurrentTime
      let sortParamSettings = map (L.sortBy (compare `on` view parameterSettingName))
      existParamSettings <- sortParamSettings <$> existingParamSettings exp
      $(logInfo) $ "Existing Parameter settings: " <> tshow (map (map showVal) $ map (\xs -> zip (L.sortBy (compare `on` parameterName) params) xs) existParamSettings)
      paramSettings <- mapM (mkParamModifications exp) params
      let paramCombs = combinations paramSettings
      let paramSingleInstances = map (filter ((== SingleInstance) . view parameterSettingExperimentDesign)) paramSettings
      let settings = filter (`notElem` existParamSettings) $ sortParamSettings $ foldl' mkSingleInstance paramCombs (concat paramSingleInstances)
          mkSingleInstance combs param =
            let occurrences = filter (param `elem`) combs
                noOccurances = filter (param `notElem`) combs
             in noOccurances ++ take 1 occurrences
      let nrs = [1 + length expsDone .. length expsDone + length settings]
      kExps <- mapM (\nr -> insert $ Exp (exp ^. experimentsKey) nr startTime Nothing) nrs
      zipWithM_ saveParamSettings kExps settings
      let exps = zipWith3 (\key nr params -> Experiment key nr startTime Nothing params []) kExps nrs settings
      unless (null exps) $ $(logInfo) $ "Created " <> tshow (length exps) <> " new experiments variations!"
      transactionSave
      return exps
  where
    showVal (ps, x) = head $ showVals (ps, [x])
    showVals (ParameterSetup _ setter getter _ _ _ _, xs) =
      let fromRight (Right x)  = x
          fromRight (Left err) = error err
       in map snd $
          map
            (\x ->
               let v = (fromRight $ S.runGet S.get $ view parameterSettingValue x)
                in (getter $ setter v (exp ^. experimentsInitialState), tshow v))
            xs


combinations :: [[a]] -> [[a]]
combinations []       = []
combinations [xs] = map return xs
combinations (xs:xss) = concatMap (\x -> map (x:) ys) xs
  where ys = combinations xss


mkParamModifications :: (MonadLogger m, MonadIO m) => Experiments a -> ParameterSetup a -> ReaderT SqlBackend m [ParameterSetting a]
mkParamModifications exps setup@(ParameterSetup n _ getter _ _ drp design) = modifyParam exps setup [] (ParameterSetting n bs (maybe False (\x -> x v) drp) (maybe FullFactory (\x -> x v) design))
  where v = getter (exps ^. experimentsInitialState)
        bs = runPut $ put v

modifyParam :: (MonadLogger m, MonadIO m) => Experiments a -> ParameterSetup a -> [ParameterSetting a] -> ParameterSetting a -> ReaderT SqlBackend m [ParameterSetting a]
modifyParam exps (ParameterSetup _ _ _ Nothing _ _ _) _ setting = return [setting]
modifyParam exps setup@(ParameterSetup n _ _ (Just modifier) mBounds drp design) acc setting = do
  case S.runGet S.get (view parameterSettingValue setting) of
    Left err -> error $ "Could not deserialize a value for parameter " <> T.unpack n <> ". Cannot proceed!"
    Right val -> do
      let filterBounds x =
            case mBounds of
              Nothing           -> True
              Just (minB, maxB) -> x <= maxB && x >= minB
          filterExperimentDesign xs = concatMap filt' $ L.groupBy ((==) `on` fst) $ L.sortBy (compare `on` fst) xs
          filt' xs@((v, _):_) =
            case design of
              Just dsgn
                | dsgn v == SingleInstance -> take 1 xs
              _ -> xs
      bss <- liftIO $ filterExperimentDesign . fmap (id &&& runPut . put) . filter filterBounds <$> modifier val
      let params' = filter (`notElem` acc) $ map (\(v, bs) -> ParameterSetting n bs (maybe False (\x -> x v) drp) (maybe FullFactory (\x -> x v) design)) bss
      foldM (modifyParam exps setup) (acc ++ params') params'


existingParamSettings :: (ExperimentDef a, MonadIO m) => Experiments a -> ReaderT SqlBackend m [[ParameterSetting a]]
existingParamSettings exp = do
  let params = parameters (exp ^. experimentsInitialState)
  expIds <- selectKeysList [ExpExps ==. exp ^. experimentsKey] []
  fmap (concatMap (toParameterSetting params . entityVal)) <$> mapM (\e -> selectList [ParamSettingExp ==. e] []) expIds
  where
    toParameterSetting params (ParamSetting _ n vBs drp dsgn) =
      case L.find ((== n) . parameterName) params of
        Nothing -> []
        _       -> [ParameterSetting n vBs drp (toEnum dsgn)]

deleteExperiment :: (MonadIO m) => Experiment a -> ReaderT SqlBackend m ()
deleteExperiment (Experiment k _ _ _ _ expRes) = mapM_ deleteExperimentResult expRes >> deleteCascade k


deleteExperimentResult :: (MonadIO m) => ExperimentResult a -> ReaderT SqlBackend m ()
deleteExperimentResult (ExperimentResult k _ _ repls) = mapM_ deleteReplicationResult repls >> deleteCascade k


-- | Loads parameters of an experiment into all initial and end states of the given experiments variable.
loadParameters  :: (ExperimentDef a) => Experiments a -> Experiment a  -> ReaderT SqlBackend (LoggingT (ExpM a)) (Experiments a)
loadParameters exps exp = foldM setParam exps (exp ^. parameterSetup)
  where
    setParam e (ParameterSetting n bs drp _) =
      case L.find (\(ParameterSetup name _ _ _ _ _ _) -> name == n) parameterSetups of
        Nothing -> do
          $(logError) $ "Could not find parameter with name " <> n <> " in the current parameter setting. Thus it cannot be modified!"
          return e
        Just (ParameterSetup _ setter _ _ _ drp _) ->
          case runGet S.get bs of
            Left err -> error $ "Could not read value of parameter " <> T.unpack n <> ". Aborting! Serializtion error was: " ++ err
            Right val -> do
              $(logInfo) $ "Loaded parameter '" <> n <> "' value: " <> tshow val
              return $ over experimentsInitialState (setter val) e
    parameterSetups = parameters (exps ^. experimentsInitialState)


type Rands = ([StdGen],[StdGen],[StdGen]) -- ^ Preparation, Warm Up and Evaluation random generators

continueExperiment :: (ExperimentDef a) => Rands -> Experiments a -> Experiment a  -> ReaderT SqlBackend (LoggingT (ExpM a)) (Updated, Experiment a)
continueExperiment rands exps exp = do
  -- TODO: parallelisation
  exps' <- loadParameters exps exp -- loads parameters into the init state
  $(logInfo) "Checking if new experiments can be created"
  expResList <- getExpRes exps' (exp ^. experimentResults) >>= truncateExperiments repetits
  $(logInfo) $ "Number of experiment results loaded: " <> tshow (length expResList)
  let dropPrep = any (^. parameterSettingDropPrepeationPhase) (exp ^. parameterSetup)
  expRes <- mapM (runExperimentResult dropPrep rands exps' expId) expResList
  let updated = any fst expRes
      res = map snd expRes
  if updated
    then do endTime <- return <$> liftIO getCurrentTime
            update (exp ^. experimentKey) [ExpEndTime =. endTime]
            return $ (updated, set experimentResults res $ set experimentEndTime endTime exp)
    else return $ (updated, set experimentResults res exp)

  where
    expId = exp ^. experimentKey
    repetits = exps ^. experimentsSetup . expsSetupRepetitions
    getExpRes :: (MonadIO m) => Experiments a -> [ExperimentResult a] -> ReaderT SqlBackend m [ExperimentResult a]
    getExpRes exps expResDone =
      (expResDone ++) <$>
      forM
        [length expResDone + 1 .. repetits]
        (\nr -> do
           startTime <- liftIO getCurrentTime
           kExpRes <- insert $ ExpResult (exp ^. experimentKey) nr Nothing
           return $ ExperimentResult kExpRes nr Nothing [])

    truncateExperiments nr xs = do
      let dels = drop nr xs
      unless (null dels) $ $(logInfo) $ "Number of experiment repetitions being deleted " <> tshow (length dels)
      mapM_ deleteExperimentResult dels
      unless (null dels) transactionSave
      return $ take nr xs

data RepResultType
  = Prep (Key ExpResult)
  | WarmUp (Key RepResult)
  | Rep (Key RepResult)


newResultData :: (ExperimentDef a) => StdGen -> RepResultType -> a -> InputState a -> ReaderT SqlBackend (LoggingT (ExpM a)) (ResultData a)
newResultData g repResType st inpSt = do
  time <- liftIO getCurrentTime
  k <- case repResType of
          Prep expResId   -> do
            serSt <- lift $ lift $ serialisable st
            prepId <- insert (PrepResultData time Nothing (tshow g) Nothing (runPut $ put serSt) Nothing (runPut $ put inpSt) Nothing)
            update expResId [ExpResultPrepResultData =. Just prepId]
            return $ ResultDataPrep prepId
          WarmUp repResId -> do
            serSt <- lift $ lift $ serialisable st
            wmUpId <- insert (WarmUpResultData time Nothing (tshow g) Nothing  (runPut $ put serSt) Nothing (runPut $ put inpSt) Nothing)
            update repResId [RepResultWarmUpResultData =. Just wmUpId]
            return $ ResultDataWarmUp wmUpId
          Rep repResId    -> do
            serSt <- lift $ lift $ serialisable st
            repResDataId <- insert (RepResultData time Nothing (tshow g) Nothing (runPut $ put serSt) Nothing (runPut $ put inpSt) Nothing)
            update repResId [RepResultRepResultData =. Just repResDataId]
            return $ ResultDataRep repResDataId
  return $ ResultData k time Nothing g Nothing (0, Available []) (0, Available []) (Available st) (Available Nothing) inpSt Nothing


type DropPreparation = Bool


runExperimentResult :: (ExperimentDef a) => DropPreparation -> Rands -> Experiments a -> Key Exp -> ExperimentResult a -> ReaderT SqlBackend (LoggingT (ExpM a)) (Updated, ExperimentResult a)
runExperimentResult dropPrep rands@(prepRands, _, _) exps expId expRes = do
  (prepUpdated, prepRes) <-
    if dropPrep
      then do
        $(logInfo) "Skipping preparation phase as provided by the parameter setting (dropPreparationPhase)."
        return (False, Nothing)
      else runPreparation (prepRands !! (expRes ^. repetitionNumber - 1)) exps expId expResId (expRes ^. preparationResults)
  repsDone <-
    if prepUpdated
      then do
        mapM_ deleteReplicationResult (expRes ^. evaluationResults)
        return []
      else return (expRes ^. evaluationResults)
  transactionSave
  mEndSt <- maybe (return Nothing) (mkTransientlyAvailable . view endState)  prepRes
  let initSt = afterPreparationPhase $ fromMaybe (exps ^. experimentsInitialState) mEndSt
      initInpSt = fromMaybe (exps ^. experimentsInitialInputState) (view endInputState =<< prepRes)
  let runRepl e repRess = do
        res <- runReplicationResult rands e expId (expRes ^. repetitionNumber) initSt initInpSt repRess
        transactionSave
        return res
  repRes <- getRepRes exps repsDone >>= mapM (runRepl exps)
  let updated = any fst repRes
      res = map snd repRes
  return (prepUpdated || updated, set preparationResults prepRes $ set evaluationResults res expRes)
  where
    expResId = expRes ^. experimentResultKey
    getRepRes :: (MonadLogger m, MonadIO m) => Experiments a -> [ReplicationResult a] -> ReaderT SqlBackend m [ReplicationResult a]
    getRepRes exps repsDone = do
      $(logInfo) $ "Number of loaded replications: " <> tshow (length repsDone)
      $(logInfo) $ "Number of new replications: " <> tshow (length [length repsDone + 1 .. exps ^. experimentsSetup . expsSetupEvaluationReplications])
      (repsDone ++) <$>
        forM
          [length repsDone + 1 .. exps ^. experimentsSetup . expsSetupEvaluationReplications]
          (\nr -> do
             kRepRes <- insert $ RepResult (expRes ^. experimentResultKey) nr Nothing Nothing
             return $ ReplicationResult kRepRes nr Nothing Nothing)


runPreparation :: (ExperimentDef a) => StdGen -> Experiments a -> Key Exp -> Key ExpResult -> Maybe (ResultData a) -> ReaderT SqlBackend (LoggingT (ExpM a)) (Updated, Maybe (ResultData a))
runPreparation g exps expId expResId mResData = do
  let len = maybe 0 (view (results._1)) mResData
  mResData' <-
    if delNeeded len
      then deleteResultData (Prep expResId) >> return Nothing
      else return mResData
  when (len > 0 && delNeeded len) $ $(logInfo) $ "Deletion of preparation data needed. Len: " <> tshow len
  when (runNeeded len) $ $(logInfo) "Preparation run is needed"
  when (not (delNeeded len) && not (runNeeded len) && prepSteps > 0) $ $(logInfo) "preparation phase needs no change"
  if runNeeded len
    then maybe new return mResData' >>= run len
    else return (delNeeded len || runNeeded len, mResData')
  where
    delNeeded len = maybe False (\r -> prepSteps < len) mResData
    runNeeded len = maybe (prepSteps > 0) (\r -> prepSteps > len || (delNeeded len && prepSteps > 0)) mResData
    prepSteps = exps ^. experimentsSetup . expsSetupPreparationSteps
    initSt = exps ^. experimentsInitialState
    initInpSt = exps ^. experimentsInitialInputState
    new = newResultData g (Prep expResId) initSt initInpSt
    run len rD = ((delNeeded len ||) *** Just) <$> runResultData expId prepSteps (Prep expResId) rD


type RepetitionNr = Int

type InitialState a = a
type InitialInputState a = InputState a

runReplicationResult ::
     (ExperimentDef a)
  => Rands
  -> Experiments a
  -> Key Exp
  -> RepetitionNr
  -> InitialState a
  -> InitialInputState a
  -> ReplicationResult a
  -> ReaderT SqlBackend (LoggingT (ExpM a)) (Updated, ReplicationResult a)
runReplicationResult (_, wmUpRands, replRands) exps expId repetNr initSt initInpSt repRes = do
  (wmUpChange, mWmUp) <- runWarmUp (wmUpRands !! ((repetNr - 1) * replicats + (repRes ^. replicationNumber - 1))) exps expId (repRes ^. replicationResultKey) initSt initInpSt (repRes ^. warmUpResults)
  initStEval <- maybe (return initSt) (\res -> fmap (fromMaybe initSt) $ mkTransientlyAvailable $ view endState res) mWmUp
  let initInpStEval = fromMaybe initInpSt (view endInputState =<< mWmUp)
  mEval <-
    if wmUpChange
      then deleteResultData (Rep repResId) >> return Nothing
      else return (repRes ^. evalResults)
  -- when wmUpChange $ $(logInfo) $ error $ "A change in the settings of the warm up phase occurred. Discarding the result data."
  (evalChange, mEval') <- runEval (replRands !! ((repetNr - 1) * replicats + (repRes ^. replicationNumber - 1))) exps expId wmUpChange (repRes ^. replicationResultKey) initStEval initInpStEval mEval
  return (wmUpChange || evalChange, set warmUpResults mWmUp $ set evalResults mEval' repRes)
  where
    repResId = repRes ^. replicationResultKey
    replicats = exps ^. experimentsSetup . expsSetupEvaluationReplications


runWarmUp ::
     (ExperimentDef a)
  => StdGen
  -> Experiments a
  -> Key Exp
  -> Key RepResult
  -> InitialState a
  -> InitialInputState a
  -> Maybe (ResultData a)
  -> ReaderT SqlBackend (LoggingT (ExpM a)) (Updated, Maybe (ResultData a))
runWarmUp g exps expId repResId initSt initInpSt mResData = do
  let len = maybe 0 (view (results._1)) mResData
  --   $(logDebug) $ "Warm up periods already run: " <> tshow len <> " Deletion needed: " <> tshow (delNeeded len)
  when (len > 0 && delNeeded len) $ $(logInfo) "Deletion of warm up data needed"
  when (runNeeded len) $ $(logInfo) "Warm up run is needed"
  when (not (delNeeded len) && not (runNeeded len) && wmUpSteps > 0) $ $(logInfo) "Warm up phase needs no change"
  mResData' <-
    if delNeeded len
      then deleteResultData (WarmUp repResId) >> return Nothing
      else return mResData
  if runNeeded len
    then maybe new return mResData' >>= run len
    else do
      when (delNeeded len) $ $(logInfo) "Deleted warm up data."
      return (delNeeded len, mResData')
  where
    delNeeded len = maybe False (\r -> wmUpSteps < len) mResData
    runNeeded len = maybe (wmUpSteps > 0) (\r -> wmUpSteps > len || (delNeeded len && wmUpSteps > 0)) mResData
    wmUpSteps = exps ^. experimentsSetup . expsSetupEvaluationWarmUpSteps
    new = newResultData g (WarmUp repResId) initSt initInpSt
    run len rD = ((delNeeded len ||) *** Just) <$> runResultData expId wmUpSteps (WarmUp repResId) rD


runEval ::
     (ExperimentDef a)
  => StdGen
  -> Experiments a
  -> Key Exp
  -> Updated
  -> Key RepResult
  -> InitialState a
  -> InitialInputState a
  -> Maybe (ResultData a)
  -> ReaderT SqlBackend (LoggingT (ExpM a)) (Updated, Maybe (ResultData a))
runEval g exps expId warmUpUpdated repResId initSt initInpSt mResData = do
  let len = maybe 0 (view (results._1)) mResData
  mResData' <-
    if delNeeded len
      then deleteResultData (Rep repResId) >> return Nothing
      else return mResData
  when (len > 0 && delNeeded len) $ $(logInfo) "Deletion of evaluation data needed"
  when (runNeeded len) $ $(logInfo) "Evaluation run is needed"
  when (not (delNeeded len) && not (runNeeded len) && evalSteps > 0) $ $(logInfo) "Evaluation phase needs no change"
  if runNeeded len
    then do
      $(logInfo) $ "An evaluation run is needed for replication with ID " <> tshow (unSqlBackendKey $ unRepResultKey repResId)
      maybe new return mResData' >>= run len
    else do
      $(logInfo) $ "No evaluation run needed for replication with ID " <> tshow (unSqlBackendKey $ unRepResultKey repResId) <> ". All needed data comes from the DB!"
      return (delNeeded len, mResData')
  where
    delNeeded len = warmUpUpdated || maybe False (\r -> evalSteps < len) mResData
    runNeeded len = maybe (evalSteps > 0) (\r -> evalSteps > len) mResData
    evalSteps = exps ^. experimentsSetup . expsSetupEvaluationSteps
    new = newResultData g (Rep repResId) initSt initInpSt
    run len rD = ((delNeeded len ||) *** Just) <$> runResultData expId evalSteps (Rep repResId) rD


deleteReplicationResult :: (MonadIO m) => ReplicationResult a -> ReaderT SqlBackend m ()
deleteReplicationResult (ReplicationResult repResId _ _ _) =
  deleteResultData (WarmUp repResId) >>
  deleteResultData (Rep repResId) >>
  deleteCascade repResId


deleteResultData :: (MonadIO m) => RepResultType -> ReaderT SqlBackend m ()
deleteResultData repResType = do
  case repResType of
    Prep expResId -> do
      update expResId [ExpResultPrepResultData =. Nothing]
      exp <- get expResId
      sequence_ $ deleteCascade <$> (view expResultPrepResultData =<< exp)
    WarmUp repResId -> do
      update repResId [RepResultWarmUpResultData =. Nothing]
      repRes <- get repResId
      sequence_ $ deleteCascade <$> (view repResultWarmUpResultData =<< repRes)
    Rep repResId -> do
      update repResId [RepResultRepResultData =. Nothing]
      repRes <- get repResId
      sequence_ $ deleteCascade <$> (view repResultRepResultData =<< repRes)


runResultData :: (ExperimentDef a) => Key Exp -> Int -> RepResultType -> ResultData a -> ReaderT SqlBackend (LoggingT (ExpM a)) (Updated, ResultData a)
runResultData expId len repResType resData = do
  startStAvail <- mkAvailable (resData ^. startState)
  st <-
    do mEndSt <- mkTransientlyAvailable (resData ^. endState)
       startSt <- mkTransientlyAvailable startStAvail
       return $ fromMaybe startSt mEndSt
  let stInp = fromMaybe (resData ^. startInputState) (resData ^. endInputState)
  let g = fromMaybe (resData ^. startRandGen) (resData ^. endRandGen)
  $(logInfo) $ "Number of steps already run is " <> tshow curLen <> ", thus still need to run " <> tshow (len - curLen) <> " steps..."
  let updated = not (null periodsToRun)
  sTime <- liftIO getCurrentTime
  (g', force -> st', force -> stInp', force -> inputs, force -> measures) <- foldM run (g, st, stInp, [], []) periodsToRun
  if updated
    then do
      eTime <- pure <$> liftIO getCurrentTime
      resData' <-
        addInputValsAndMeasure (reverse inputs) (reverse measures) $
        (if isNew
           then set startTime sTime
           else id) $
        set endInputState (Just stInp') $
        set endState (Available $ Just st') $
        set startState startStAvail $ -- make available once for saving
        set endRandGen (Just g') $
        set endTime eTime resData
      upd repResType resData'
      transactionSave
      if len - curLen - length periodsToRun > 0
        then runResultData expId len repResType resData'
        else do
          $(logInfo) "Done and saved. Releasing memory."
          return (True, set endState mkEndStateAvailableOnDemand $ set startState mkStartStateAvailableOnDemand resData')
    else return (False, resData)
  where
    curLen = resData ^. results . _1
    isNew = curLen == 0
    periodsToRun = [1 + curLen .. curLen + min 1000 len]
    periodsToRunLen = length periodsToRun
    mkEndStateAvailableOnDemand =
      case resData ^. resultDataKey of
        ResultDataPrep key -> AvailableOnDemand $ loadResDataEndState expId (view prepResultDataEndState) key
        ResultDataWarmUp key -> AvailableOnDemand $ loadResDataEndState expId (view warmUpResultDataEndState) key
        ResultDataRep key -> AvailableOnDemand $ loadResDataEndState expId (view repResultDataEndState) key
    mkStartStateAvailableOnDemand =
      case resData ^. resultDataKey of
        ResultDataPrep key -> AvailableOnDemand $ loadResDataStartState expId (view prepResultDataStartState) key
        ResultDataWarmUp key -> AvailableOnDemand $ loadResDataStartState expId (view warmUpResultDataStartState) key
        ResultDataRep key -> AvailableOnDemand $ loadResDataStartState expId (view repResultDataStartState) key
    addInputValsAndMeasure inputVals measures resData =
      let countResults' = resData ^. results . _1 + length measures
          countInputValues' = resData ^. inputValues . _1 + length inputVals
       in case resData ^. resultDataKey of
            ResultDataPrep key -> do
              inpKeys <- mapM (insert . PrepInput key . view inputValuePeriod) inputVals
              zipWithM_ (\k v -> insert $ PrepInputValue k (runPut . put . view inputValue $ v)) inpKeys inputVals
              measureKeys <- mapM (insert . PrepMeasure key . view measurePeriod) measures
              zipWithM_ (\k (Measure _ xs) -> mapM (\(StepResult n mX y) -> insert $ PrepResultStep k n mX y) xs) measureKeys measures
              return $ results .~ (countResults', AvailableOnDemand (loadPrepartionMeasures key)) $ inputValues .~
                (countInputValues', AvailableOnDemand (fromMaybe [] <$> loadPreparationInput key)) $
                resData
            ResultDataWarmUp key -> do
              inpKeys <- mapM (insert . WarmUpInput key . view inputValuePeriod) inputVals
              zipWithM_ (\k v -> insert $ WarmUpInputValue k (runPut . put . view inputValue $ v)) inpKeys inputVals
              measureKeys <- mapM (insert . WarmUpMeasure key . view measurePeriod) measures
              zipWithM_ (\k (Measure _ xs) -> mapM (\(StepResult n mX y) -> insert $ WarmUpResultStep k n mX y) xs) measureKeys measures
              return $ results .~ (countResults', AvailableOnDemand (loadReplicationWarmUpMeasures key)) $ inputValues .~
                (countInputValues', AvailableOnDemand (fromMaybe [] <$> loadReplicationWarmUpInput key)) $
                resData
            ResultDataRep key -> do
              inpKeys <- mapM (insert . RepInput key . view inputValuePeriod) inputVals
              zipWithM_ (\k v -> insert $ RepInputValue k (runPut . put . view inputValue $ v)) inpKeys inputVals
              measureKeys <- mapM (insert . RepMeasure key . view measurePeriod) measures
              zipWithM_ (\k (Measure _ xs) -> mapM (\(StepResult n mX y) -> insert $ RepResultStep k n mX y) xs) measureKeys measures
              return $ results .~ (countResults', AvailableOnDemand (loadReplicationMeasures key)) $ inputValues .~
                (countInputValues', AvailableOnDemand (fromMaybe [] <$> loadReplicationInput key)) $
                resData
    upd :: (ExperimentDef a) => RepResultType -> ResultData a -> ReaderT SqlBackend (LoggingT (ExpM a)) ()
    upd (Prep expResId) (ResultData (ResultDataPrep k) sTime eTime sG eG inpVals ress sSt eSt sInpSt eInpSt) = do
      serSt <- mkTransientlyAvailable sSt >>= lift . lift . serialisable
      serESt <- mkTransientlyAvailable eSt >>= lift . lift . sequence . fmap serialisable
      replace k (PrepResultData sTime eTime (tshow sG) (tshow <$> eG) (runPut . put $ serSt) (runPut . put <$> serESt) (runPut . put $ sInpSt) (runPut . put <$> eInpSt))
    upd (WarmUp repResId) (ResultData (ResultDataWarmUp k) sTime eTime sG eG inpVals ress sSt eSt sInpSt eInpSt) = do
      serSt <- mkTransientlyAvailable sSt >>= lift . lift . serialisable
      serESt <- mkTransientlyAvailable eSt >>= lift . lift . sequence . fmap serialisable
      replace k (WarmUpResultData sTime eTime (tshow sG) (tshow <$> eG) (runPut . put $ serSt) (runPut . put <$> serESt) (runPut . put $ sInpSt) (runPut . put <$> eInpSt))
    upd (Rep repResId) (ResultData (ResultDataRep k) sTime eTime sG eG inpVals ress sSt eSt sInpSt eInpSt) = do
      serSt <- mkTransientlyAvailable sSt >>= lift . lift . serialisable
      serESt <- mkTransientlyAvailable eSt >>= lift . lift . sequence . fmap serialisable
      replace k (RepResultData sTime eTime (tshow sG) (tshow <$> eG) (runPut . put $ serSt) (runPut . put <$> serESt) (runPut . put $ sInpSt) (runPut . put <$> eInpSt))
    upd _ _ = error "Unexpected update combination. This is a bug, please report it!"


run :: (ExperimentDef a) => (StdGen, a, InputState a, [Input a], [Measure]) -> Int -> ReaderT SqlBackend (LoggingT (ExpM a)) (StdGen, a, InputState a, [Input a], [Measure])
run (g, st, stInp, inpVals, res) period = do
  let (randGen, g') = split g
  (inpVal', inpSt') <- lift $ lift $ generateInput randGen st stInp period
  (res', st') <- lift $ lift $ runStep st inpVal' period
  return (g', st', inpSt', Input period inpVal' : inpVals, Measure period res' : res)
