{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Experimenter.Run
    ( DatabaseSetting (..)
    , MkExperimentSetting
    , execExperiments
    , runExperiments
    , runExperimentsM
    , runExperimentsIO
    , loadExperimentsResultsM
    ) where

import           Control.Arrow                (first, second, (&&&), (***))
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad                (forM)
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger         (LogLevel (..), LoggingT, MonadLogger,
                                               NoLoggingT, WriterLoggingT, defaultLoc,
                                               filterLogger, logDebug, logError, logInfo,
                                               runFileLoggingT, runLoggingT, runNoLoggingT,
                                               runStderrLoggingT, runStdoutLoggingT)
import           Data.IORef
import           Data.List                    (foldl')

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as BS
import           Data.Function                (on)
import           Data.Int                     (Int64)
import qualified Data.List                    as L
import           Data.Maybe                   (fromJust, fromMaybe, isJust, isNothing)
import           Data.Pool                    as P
import           Data.Serialize               hiding (get)
import qualified Data.Serialize               as S
import qualified Data.Text                    as T
import           Data.Time                    (UTCTime, addUTCTime, diffUTCTime,
                                               getCurrentTime)
import qualified Database.Esqueleto           as E
import           Database.Persist.Postgresql
import           Database.Persist.Sql         (fromSqlKey, toSqlKey)
import           Network.HostName             (HostName, getHostName)
import           System.IO
import           System.Posix.Process
import           System.Random.MWC


import           Experimenter.DatabaseSetting
import           Experimenter.Experiment
import           Experimenter.Input
import           Experimenter.MasterSlave
import           Experimenter.Measure
import           Experimenter.Models
import           Experimenter.Parameter
import           Experimenter.Result
import           Experimenter.Setting
import           Experimenter.StepResult
import           Experimenter.Util


import           Debug.Trace

type Updated = Bool
type InitialState a = a
type InitialInputState a = InputState a
type SkipPreparation = Bool
type Rands = ([Seed],[Seed],[Seed]) -- ^ Preparation, Warm Up and Evaluation random generators

data Mode = Master | Slave
  deriving (Eq, Show)

execExperiments :: (ExperimentDef a) => (ExpM a (Bool, Experiments a) -> IO (Bool, Experiments a)) -> DatabaseSetting -> MkExperimentSetting a -> InputState a -> a -> IO (Experiments a)
execExperiments runExpM dbSetup setup initInpSt initSt = force . snd <$> runExperiments runExpM dbSetup setup initInpSt initSt

-- | Run an experiment with non-monadic initial state. In case the initial state requires monadic effect (e.g. building
-- a Tensorflow model), use `runExperimentsM`!
runExperiments :: (ExperimentDef a) => (ExpM a (Bool, Experiments a) -> IO (Bool, Experiments a)) -> DatabaseSetting -> MkExperimentSetting a -> InputState a -> a -> IO (Bool, Experiments a)
runExperiments runExpM dbSetup setup initInpSt initSt = force <$> runner runExpM dbSetup setup initInpSt (return initSt)

runExperimentsM :: (ExperimentDef a) => (ExpM a (Bool, Experiments a) -> IO (Bool, Experiments a)) -> DatabaseSetting -> MkExperimentSetting a -> InputState a -> ExpM a a -> IO (Bool, Experiments a)
runExperimentsM = runner

runExperimentsIO :: (ExperimentDef a, IO ~ ExpM a) => DatabaseSetting -> MkExperimentSetting a -> InputState a -> a -> IO (Bool, Experiments a)
runExperimentsIO dbSetup setup initInpSt initSt = runner id dbSetup setup initInpSt (return initSt)

runner :: (ExperimentDef a) => (ExpM a (Bool, Experiments a) -> IO (Bool, Experiments a)) -> DatabaseSetting -> MkExperimentSetting a -> InputState a -> ExpM a a -> IO (Bool, Experiments a)
runner runExpM dbSetup setup initInpSt mkInitSt =
  fmap force $ do
    runStdoutLoggingT $ withPostgresqlPool (connectionString dbSetup) (parallelConnections dbSetup) $ liftSqlPersistMPool $ runMigration migrateAll
    runExpM $
      (runStdoutLoggingT . filterLogger (\s _ -> s /= "SQL")) $
      withPostgresqlConn (connectionString dbSetup) $ \backend ->
        flip runSqlConn backend $ do
          initSt <- lift (lift mkInitSt)
          let setting = setup initSt
          loadExperiments setting initInpSt initSt >>= checkUniqueParamNames >>= runExperimenter dbSetup setting initInpSt initSt

loadExperimentsResultsM :: (ExperimentDef a) => (ExpM a (Maybe (Experiments a)) -> IO (Maybe (Experiments a))) -> DatabaseSetting -> MkExperimentSetting a -> InputState a -> ExpM a a -> Int64 -> IO (Maybe (Experiments a))
loadExperimentsResultsM runExpM dbSetup setup initInpSt mkInitSt key =
  runExpM $
  (runStdoutLoggingT . filterLogger (\s _ -> s /= "SQL")) $
  withPostgresqlConn (connectionString dbSetup) $ \backend ->
    flip runSqlConn backend $ do
      initSt <- lift (lift mkInitSt)
      let setting = setup initSt
          isFinished exp =
            length (exp ^. experimentResults) == setting ^. experimentRepetitions && -- repetitions
            all (\expRes -> maybe 0 (lengthAvailabilityList . view results) (expRes ^. preparationResults) == setting ^. preparationSteps) (exp ^. experimentResults) && -- preparation length
            all (\expRes -> length (expRes ^. evaluationResults) == setting ^. evaluationReplications) (exp ^. experimentResults) && -- replications
            all (\expRes -> maybe 0 (lengthAvailabilityList . view results) (expRes ^. warmUpResults) == setting ^. evaluationWarmUpSteps) (exp ^. experimentResults.traversed.evaluationResults) && -- warm up length
            all (\expRes -> maybe 0 (lengthAvailabilityList . view results) (expRes ^. evalResults) == setting ^. evaluationSteps) (exp ^. experimentResults.traversed.evaluationResults) -- eval length
          filterFinished x = over experiments (filter isFinished) x


      fmap filterFinished <$> loadExperimentsResults setting initInpSt initSt (toSqlKey key)


checkUniqueParamNames :: (Monad m) => Experiments a -> m (Experiments a)
checkUniqueParamNames exps = do
  let paramNames = map parameterName (view experimentsParameters exps)
  when (any ((> 1) . length) (L.group $ L.sort paramNames)) $ error "Parameter names must be unique!"
  return exps


runExperimenter :: (ExperimentDef a) => DatabaseSetting -> ExperimentSetting -> InputState a -> a -> Experiments a -> ReaderT SqlBackend (LoggingT (ExpM a)) (Bool, Experiments a)
runExperimenter dbSetup setup initInpSt initSt exps = do
  pid <- liftIO getProcessID
  hostName <- liftIO getHostName
  time <- liftIO getCurrentTime
  deleteWhere [ExpsMasterLastAliveSign <=. addUTCTime (-2*keepAliveTimeout) time]
  maybeMaster <- insertUnique $ ExpsMaster (exps ^. experimentsKey) (T.pack hostName) (fromIntegral pid) time
  transactionSave
  case maybeMaster of
    Just masterId -> do
      ref <- liftIO $ createKeepAliveFork dbSetup (\t -> update masterId [ExpsMasterLastAliveSign =. t]) (delete masterId)
      $(logInfo) "Running in MASTER mode!"
      res <- runExperiment dbSetup Master exps
      waitResult <- waitForSlaves exps
      if waitResult
        then do
          liftIO (writeIORef ref Finished)
          exps' <- loadExperiments setup initInpSt initSt -- done, reload all data!
          return (fst res, exps')
        else delete masterId >> restartExperimenter -- slave died
    Nothing -> do
      $(logInfo) "Running in SLAVE mode!"
      runExperiment dbSetup Slave exps
  where
    restartExperimenter = loadExperiments setup initInpSt initSt >>= runExperimenter dbSetup setup initInpSt initSt

runExperiment :: (ExperimentDef a) => DatabaseSetting -> Mode -> Experiments a -> ReaderT SqlBackend (LoggingT (ExpM a)) (Bool, Experiments a)
runExperiment dbSetup mode exps = do
  (anyChange, exps') <- continueExperiments dbSetup mode exps
  if anyChange
    then first (const True) <$> runExperiment dbSetup mode exps'
    else return (anyChange, exps')


continueExperiments :: (ExperimentDef a) => DatabaseSetting -> Mode -> Experiments a -> ReaderT SqlBackend (LoggingT (ExpM a)) (Bool, Experiments a)
continueExperiments dbSetup mode exp = do
  $(logInfo) $ "Processing set of experiments with ID " <> tshow (unSqlBackendKey $ unExpsKey $ exp ^. experimentsKey)
  liftIO $ hFlush stdout
  let exps = exp ^. experiments
  if mode == Slave && null exps
    then do
      $(logInfo) "No experiments found and running in slave mode. Check whether the master has initialised the experiment yet!"
      return (False, exp)
    else do
      printInfoParamSetup
      rands <- liftIO $ mkRands exps
      newExps <-
        if mode == Slave
          then return []
          else mkNewExps exp exps
      let expsList = exps ++ newExps
      $(logInfo) $ "Number of experiments loaded: " <> tshow (length exps)
      $(logInfo) $ "Number of new experiments: " <> tshow (length newExps)
      expRes <- mapM (continueExperiment dbSetup rands exp) expsList
      let updated = any fst expRes
          res = map snd expRes
      if updated
        then do
          endTime <- return <$> liftIO getCurrentTime
          update (exp ^. experimentsKey) [ExpsEndTime =. endTime]
          return (updated, set experiments res $ set experimentsEndTime endTime exp)
        else return (updated, set experiments res exp)
  where
    printInfoParamSetup = do
      $(logInfo) "------------------------------"
      $(logInfo) "--   INFO PARAMETER SETUP   --"
      $(logInfo) "------------------------------"
      if null (view experimentsInfoParameters exp)
        then $(logDebug) "No info parameters set."
        else mapM_ printInfoParam (view experimentsInfoParameters exp)
      $(logInfo) "------------------------------"
    printInfoParam (ExperimentInfoParameter p v) = $(logInfo) $ p <> ": " <> tshow v
    mkRands :: [Experiment a] -> IO Rands
    mkRands [] = do
      prep <- replicateM repetits (createSystemRandom >>= save)
      wmUp <- replicateM (repetits * replicats) (createSystemRandom >>= save)
      repl <- replicateM (repetits * replicats) (createSystemRandom >>= save)
      return (prep, wmUp, repl)
    mkRands (x:_) = do
      currentPrep <- mapM save (x ^.. experimentResults . traversed . preparationResults . traversed . startRandGen)
      currentWmUp <- mapM save (x ^.. experimentResults . traversed . evaluationResults . traversed . warmUpResults . traversed . startRandGen)
      currentRepl <- mapM save (x ^.. experimentResults . traversed . evaluationResults . traversed . evalResults . traversed . startRandGen)
      prepNew <- replicateM (repetits - length currentPrep) (createSystemRandom >>= save)
      wmUpNew <- replicateM (repetits * replicats - length currentWmUp) (createSystemRandom >>= save)
      replNew <- replicateM (repetits * replicats - length currentRepl) (createSystemRandom >>= save)
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
      -- $(logInfo) $ "Existing Parameter settings: " <> tshow (map (map showVal) $ map (\xs -> zip (L.sortBy (compare `on` parameterName) params) xs) existParamSettings)
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
  -- where
  --   showVal (ps, x) = head $ showVals (ps, [x])
  --   showVals (ParameterSetup _ setter getter _ _ _ _, xs) =
  --     let fromRight (Right x)  = x
  --         fromRight (Left err) = error err
  --      in map snd $
  --         map
  --           (\x ->
  --              let v = (fromRight $ S.runGet S.get $ view parameterSettingValue x)
  --               in (getter $ setter v (exp ^. experimentsInitialState), tshow v))
  --           xs


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


continueExperiment :: (ExperimentDef a) => DatabaseSetting -> Rands -> Experiments a -> Experiment a  -> ReaderT SqlBackend (LoggingT (ExpM a)) (Updated, Experiment a)
continueExperiment dbSetup rands exps expIn = do
  pid <- liftIO getProcessID
  hostName <- liftIO getHostName
  time <- liftIO getCurrentTime
  deleteWhere [ExpExecutionLockLastAliveSign <=. addUTCTime (-2 * keepAliveTimeout) time]
  maybeLocked <- insertUnique $ ExpExecutionLock expId (T.pack hostName) (fromIntegral pid) time
  transactionSave
  case maybeLocked of
    Nothing -> do
      $(logInfo) $ "Skipping experiment with ID " <> tshow (fromSqlKey expId) <> " as it is currently locked by another worker."
      return (False, expIn)
    Just lock -> do
      expResults <- loadExperimentResults expId -- update data
      let exp = set experimentResults expResults expIn
      printParamSetup exp
      $(logInfo) $ "Processing experiment with ID " <> tshow (fromSqlKey expId) <> "."
      ref <- liftIO $ createKeepAliveFork dbSetup (\t -> update lock [ExpExecutionLockLastAliveSign =. t]) (delete lock)
      liftIO (writeIORef ref Working)
      exps' <- loadParameters exps exp -- loads parameters into the init state
      $(logInfo) "Checking if new experiments can be created"
      !expResList <- force <$> (getExpRes exps' (exp ^. experimentResults) >>= truncateExperiments repetits)
      $(logInfo) $ "Number of experiment results loaded: " <> tshow (length expResList)
      let skipPrep = any (^. parameterSettingSkipPreparationPhase) (exp ^. parameterSetup)
      expRes <- force <$> mapM (runExperimentResult skipPrep rands exps' expId expNr) expResList
      let updated = any fst expRes
          res = map snd expRes
      expRes `seq` liftIO (writeIORef ref Finished)
      if updated
        then do
          eTime <- return <$> liftIO getCurrentTime
          update (exp ^. experimentKey) [ExpEndTime =. eTime]
          return (updated, set experimentResults res $ set experimentEndTime eTime exp)
        else return (updated, set experimentResults res exp)
  where
    printParamSetup exp = do
      $(logInfo) "------------------------------"
      $(logInfo) "--  LOADED PARAMETER SETUP  --"
      $(logInfo) "------------------------------"
      if null (view parameterSetup expIn)
        then $(logDebug) "No info parameters set."
        else mapM_ (printParamSetting exps) (view parameterSetup exp)
      $(logInfo) "------------------------------"
    expNr = expIn ^. experimentNumber
    expId = expIn ^. experimentKey
    repetits = exps ^. experimentsSetup . expsSetupRepetitions
    getExpRes :: (MonadIO m) => Experiments a -> [ExperimentResult a] -> ReaderT SqlBackend m [ExperimentResult a]
    getExpRes exps expResDone =
      (expResDone ++) <$>
      forM
        [length expResDone + 1 .. repetits]
        (\nr -> do
           kExpRes <- insert $ ExpResult expId nr Nothing
           return $ ExperimentResult kExpRes nr Nothing [])
    truncateExperiments nr xs = do
      let dels = drop nr xs
      unless (null dels) $ $(logInfo) $ "Number of experiment repetitions being deleted " <> tshow (length dels)
      mapM_ deleteExperimentResult dels
      unless (null dels) transactionSave
      return $ take nr xs

printParamSetting :: (ExperimentDef a) => Experiments a -> ParameterSetting a -> ReaderT SqlBackend (LoggingT (ExpM a)) ()
printParamSetting exps (ParameterSetting n bs skipPrep expDes) =
  case L.find ((== n) . parameterName) (exps ^. experimentsParameters) of
    (Just (ParameterSetup _ setter _ _ _ _ _)) ->
      case S.runGet S.get bs of
        Left _ -> err
        Right val -> do
          let _ = setter val (exps ^. experimentsInitialState) -- only needed for type inference
          $(logInfo) $
            n <> ": " <> tshow val <>
            if skipPrep
              then " [SkipPreparation] "
              else "" <>
                   if expDes == SingleInstance
                     then " [SingleInstance] "
                     else ""
    _ -> err
  where
    err = $(logInfo) $ n <> ": Could not deserialise value as this parameter does not exist anymore. Thus keeping it unchanged."


data RepResultType
  = Prep (Key ExpResult)
  | WarmUp (Key RepResult)
  | Rep (Key RepResult)


newResultData :: (ExperimentDef a) => Seed -> RepResultType -> a -> InputState a -> ReaderT SqlBackend (LoggingT (ExpM a)) (ResultData a)
newResultData seed repResType st inpSt = do
  time <- liftIO getCurrentTime
  k <- case repResType of
          Prep expResId   -> do
            serSt <- lift $ lift $ serialisable st
            prepId <- insert (PrepResultData time Nothing (serialiseSeed seed) Nothing (runPut $ put inpSt) Nothing)
            setResDataStartState (StartStatePrep prepId) (runPut $ put serSt)
            update expResId [ExpResultPrepResultData =. Just prepId]
            return $ ResultDataPrep prepId
          WarmUp repResId -> do
            serSt <- lift $ lift $ serialisable st
            wmUpId <- insert (WarmUpResultData time Nothing (serialiseSeed seed) Nothing  (runPut $ put inpSt) Nothing)
            setResDataStartState (StartStateWarmUp wmUpId) (runPut $ put serSt)
            update repResId [RepResultWarmUpResultData =. Just wmUpId]
            return $ ResultDataWarmUp wmUpId
          Rep repResId    -> do
            serSt <- lift $ lift $ serialisable st
            repResDataId <- insert (RepResultData time Nothing (serialiseSeed seed) Nothing (runPut $ put inpSt) Nothing)
            setResDataStartState (StartStateRep repResDataId) (runPut $ put serSt)
            update repResId [RepResultRepResultData =. Just repResDataId]
            return $ ResultDataRep repResDataId
  g <- liftIO $ restore seed
  return $ ResultData k time Nothing g Nothing (0, Available []) (0, Available []) (Available st) (Available Nothing) inpSt Nothing


runExperimentResult ::
     (ExperimentDef a)
  => SkipPreparation
  -> Rands
  -> Experiments a
  -> Key Exp
  -> ExperimentNumber
  -> ExperimentResult a
  -> ReaderT SqlBackend (LoggingT (ExpM a)) (Updated, ExperimentResult a)
runExperimentResult skipPrep rands@(prepRands, _, _) exps expId expNr expRes = do
  let repetNr = expRes ^. repetitionNumber
  let prepSeed = prepRands !! (repetNr - 1)
  (prepInitSt, delPrep) <-
    maybe (return (expInitSt, False)) (fmap (maybe (expInitSt, True) (, False)) . mkTransientlyAvailable) (expRes ^? preparationResults . traversed . endState)
  (prepUpdated, prepRes) <-
    if skipPrep
      then do
        $(logInfo) "Skipping preparation phase as provided by the parameter setting (skipPreparationPhase)."
        return (False, Nothing)
      else runPreparation prepSeed exps expId expResId (expNr, repetNr) delPrep prepInitSt (expRes ^. preparationResults)
  repsDone <-
    if prepUpdated
      then do
        mapM_ deleteReplicationResult (expRes ^. evaluationResults)
        return []
      else return (expRes ^. evaluationResults)
  transactionSave
  mEndSt <- maybe (return Nothing) (mkTransientlyAvailable . view endState) prepRes
  let initSt = fromMaybe expInitSt mEndSt
  let initInpSt = fromMaybe (exps ^. experimentsInitialInputState) (view endInputState =<< prepRes)
  let runRepl e repRess = do
        res <- runReplicationResult rands e expId (expNr, repetNr) initSt initInpSt repRess
        transactionSave
        return res
  repRes <- getRepRes exps repsDone >>= mapM (runRepl exps)
  let updated = any fst repRes
      res = map snd repRes
  return (prepUpdated || updated, set preparationResults prepRes $ set evaluationResults res expRes)
  where
    expInitSt = exps ^. experimentsInitialState
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


runPreparation ::
     (ExperimentDef a)
  => Seed
  -> Experiments a
  -> Key Exp
  -> Key ExpResult
  -> (ExperimentNumber, RepetitionNumber)
  -> Bool
  -> a
  -> Maybe (ResultData a)
  -> ReaderT SqlBackend (LoggingT (ExpM a)) (Updated, Maybe (ResultData a))
runPreparation seed exps expId expResId (expNr, repetNr) prepDelNeeded prepInitSt mResData = do
  g <- liftIO $ restore seed
  initSt <- lift $ lift $ beforePreparationHook expNr repetNr g prepInitSt
  let len = maybe 0 (view (results . _1)) mResData
  mResData' <-
    if delNeeded len
      then deleteResultData (Prep expResId) >> return Nothing
      else return mResData
  when (len > 0 && delNeeded len) $ $(logInfo) $ "Deletion of preparation data needed. Len: " <> tshow len
  when (runNeeded len) $ $(logInfo) "Preparation run is needed"
  when (not (delNeeded len) && not (runNeeded len) && prepSteps > 0) $ $(logInfo) "preparation phase needs no change"
  if runNeeded len
    then do
      res <- maybe (new initSt) return mResData' >>= run len
      liftIO $ afterPreparationHook initSt expNr repetNr
      return res
    else return (delNeeded len || runNeeded len, mResData')
  where
    delNeeded len = prepDelNeeded || maybe False (\_ -> prepSteps < len) mResData
    runNeeded len = maybe (prepSteps > 0) (\_ -> prepSteps > len || (delNeeded len && prepSteps > 0)) mResData
    prepSteps = exps ^. experimentsSetup . expsSetupPreparationSteps
    initInpSt = exps ^. experimentsInitialInputState
    new initSt = newResultData seed (Prep expResId) initSt initInpSt
    run len rD = ((delNeeded len ||) *** Just) <$> runResultData expId prepSteps (Prep expResId) rD


runReplicationResult ::
     (ExperimentDef a)
  => Rands
  -> Experiments a
  -> Key Exp
  -> (ExperimentNumber, RepetitionNumber)
  -> InitialState a
  -> InitialInputState a
  -> ReplicationResult a
  -> ReaderT SqlBackend (LoggingT (ExpM a)) (Updated, ReplicationResult a)
runReplicationResult (_, wmUpRands, replRands) exps expId (expNr, repetNr) initSt initInpSt repRes = do
  let repliNr = repRes ^. replicationNumber
  let randGenIdx = (repetNr - 1) * replicats + (repliNr - 1)
  let wmUpRand = wmUpRands !! randGenIdx
  let repRand = replRands !! randGenIdx
  $(logInfo) $ "Running replication " <> tshow repliNr <> " for experiment repetition " <> tshow repetNr
  (wmUpChange, mWmUp) <- runWarmUp wmUpRand exps expId (repRes ^. replicationResultKey) (expNr, repetNr, repliNr) initSt initInpSt (repRes ^. warmUpResults)
  initStEval <- maybe (return initSt) (fmap (fromMaybe initSt) . mkTransientlyAvailable . view endState) mWmUp
  let initInpStEval = fromMaybe initInpSt (view endInputState =<< mWmUp)
  mEval <-
    if wmUpChange
      then deleteResultData (Rep repResId) >> return Nothing
      else return (repRes ^. evalResults)
  (evalChange, mEval') <- runEval repRand exps expId wmUpChange (repRes ^. replicationResultKey) (expNr, repetNr, repliNr) initStEval initInpStEval mEval
  return (wmUpChange || evalChange, set warmUpResults mWmUp $ set evalResults mEval' repRes)
  where
    repResId = repRes ^. replicationResultKey
    replicats = exps ^. experimentsSetup . expsSetupEvaluationReplications


runWarmUp ::
     (ExperimentDef a)
  => Seed
  -> Experiments a
  -> Key Exp
  -> Key RepResult
  -> (ExperimentNumber, RepetitionNumber, ReplicationNumber)
  -> InitialState a
  -> InitialInputState a
  -> Maybe (ResultData a)
  -> ReaderT SqlBackend (LoggingT (ExpM a)) (Updated, Maybe (ResultData a))
runWarmUp seed exps expId repResId (expNr, repetNr, repliNr) initSt initInpSt mResData = do
  g <- liftIO $ restore seed
  initStWmUp <- lift $ lift $ beforeWarmUpHook expNr repetNr repliNr g initSt
  let len = maybe 0 (view (results . _1)) mResData
  when (len > 0 && delNeeded len) $ $(logInfo) "Deletion of warm up data needed"
  when (runNeeded len) $ $(logInfo) "Warm up run is needed"
  when (not (delNeeded len) && not (runNeeded len) && wmUpSteps > 0) $ $(logInfo) "Warm up phase needs no change"
  mResData' <-
    if delNeeded len
      then deleteResultData (WarmUp repResId) >> return Nothing
      else return mResData
  if runNeeded len
    then do
      res <- maybe (new initStWmUp) return mResData' >>= run len
      liftIO $ afterWarmUpHook initSt expNr repetNr repliNr
      return res
    else do
      when (delNeeded len) $ $(logInfo) "Deleted warm up data."
      return (delNeeded len, mResData')
  where
    delNeeded len = maybe False (\_ -> wmUpSteps < len) mResData --  || maybe False ((>0) . lengthAvailabilityList) (mResData ^? traversed.results)
    runNeeded len = maybe (wmUpSteps > 0) (\_ -> wmUpSteps > len || (delNeeded len && wmUpSteps > 0)) mResData
    wmUpSteps = exps ^. experimentsSetup . expsSetupEvaluationWarmUpSteps
    new initStWmUp = newResultData seed (WarmUp repResId) initStWmUp initInpSt
    run len rD = ((delNeeded len ||) *** Just) <$> runResultData expId wmUpSteps (WarmUp repResId) rD


runEval ::
     (ExperimentDef a)
  => Seed
  -> Experiments a
  -> Key Exp
  -> Updated
  -> Key RepResult
  -> (ExperimentNumber, RepetitionNumber, ReplicationNumber)
  -> InitialState a
  -> InitialInputState a
  -> Maybe (ResultData a)
  -> ReaderT SqlBackend (LoggingT (ExpM a)) (Updated, Maybe (ResultData a))
runEval seed exps expId warmUpUpdated repResId (expNr, repetNr, repliNr) initSt initInpSt mResData = do
  g <- liftIO $ restore seed
  initStEval <- lift $ lift $ beforeEvaluationHook expNr repetNr repliNr g initSt
  let len = maybe 0 (view (results . _1)) mResData
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
      res <- maybe (new initStEval) return mResData' >>= run len
      liftIO $ afterEvaluationHook initSt expNr repetNr repliNr
      return res
    else do
      $(logInfo) $ "No evaluation run needed for replication with ID " <> tshow (unSqlBackendKey $ unRepResultKey repResId) <> ". All needed data comes from the DB!"
      return (delNeeded len, mResData')
  where
    delNeeded len = warmUpUpdated || maybe False (\_ -> evalSteps < len) mResData --  || maybe False ((>0) . lengthAvailabilityList) (mResData ^? traversed.results)
    runNeeded len = maybe (evalSteps > 0) (\_ -> evalSteps > len) mResData
    evalSteps = exps ^. experimentsSetup . expsSetupEvaluationSteps
    new initStEval = newResultData seed (Rep repResId) initStEval initInpSt
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

foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ acc [] = return acc
foldM' f acc (x:xs) = do
  !acc' <- f acc x
  acc' `seq` foldM' f acc' xs


runResultData :: (ExperimentDef a) => Key Exp -> Int -> RepResultType -> ResultData a -> ReaderT SqlBackend (LoggingT (ExpM a)) (Updated, ResultData a)
runResultData expId len repResType resData = do
  ~startStAvail <- mkAvailable (resData ^. startState)
  st <- mkTransientlyAvailable (resData ^. endState) >>= maybe (mkTransientlyAvailable startStAvail) return
  let stInp = fromMaybe (resData ^. startInputState) (resData ^. endInputState)
  let g = fromMaybe (resData ^. startRandGen) (resData ^. endRandGen)
  $(logInfo) $ "Number of steps already run is " <> tshow curLen <> ", thus still need to run " <> tshow (len - curLen) <> " steps."
  let updated = not (null periodsToRun)
  sTime <- liftIO getCurrentTime
  let phase = fromEnum $ phaseFromResultDataKey (resData ^. resultDataKey)
  void $ upsertBy (UniqueExpProgress expId) (ExpProgress expId phase curLen) [ExpProgressPhase =. phase, ExpProgressStep =. curLen]
  (g', force -> st', force -> stInp', inputs, force -> measures) <- foldM' run (g, st, stInp, [], []) periodsToRun
  when updated $ void $ upsertBy (UniqueExpProgress expId) (ExpProgress expId phase (curLen + nrOfPeriodsToRun)) [ExpProgressPhase =. phase, ExpProgressStep =. curLen + nrOfPeriodsToRun]
  if updated
    then do
      eTime <- pure <$> liftIO getCurrentTime
      sTime' <- liftIO getCurrentTime
      resData' <-
        fmap force $ addInputValsAndMeasure (reverse inputs) (reverse measures) $ doIf isNew (set startTime sTime) $ set endInputState (Just stInp') $
        set endState (Available $ Just st') $
        doIf isNew (set startState startStAvail) $ -- make available once for saving
        set endRandGen (Just g') $
        set endTime eTime resData
      upd repResType resData'
      transactionSave
      eTime' <- liftIO getCurrentTime
      if len - curLen - length periodsToRun > 0
        then do
          $(logInfo) $ "Computation Time of " <> tshow (length periodsToRun) <> ": " <> tshow (diffUTCTime (fromJust eTime) sTime) <> ". Saving Time: " <>
            tshow (diffUTCTime eTime' sTime')
          runResultData expId len repResType resData'
        else do
          $(logInfo) $ "Done and saved. Computation Time of " <> tshow (length periodsToRun) <> ": " <> tshow (diffUTCTime (fromJust eTime) sTime) <> ". Saving Time: " <>
            tshow (diffUTCTime eTime' sTime') <>
            ". Releasing memory."
          return (True, set endState mkEndStateAvailableOnDemand $ set startState mkStartStateAvailableOnDemand resData')
    else return (False, resData)
  where
    doIf pred f
      | pred = f
      | otherwise = id
    curLen = resData ^. results . _1
    delInputs = resData ^. results . _1 - resData ^. inputValues . _1 > 0
    isNew = curLen == 0
    splitPeriods = 5000
    nrOfPeriodsToRun = min splitPeriods (len - curLen)
    periodsToRun = map (+ curLen) [1 .. nrOfPeriodsToRun]
    mkStartStateAvailableOnDemand =
      case resData ^. resultDataKey of
        ResultDataPrep key -> AvailableOnDemand $ loadResDataStartState expId (StartStatePrep key)
        ResultDataWarmUp key -> AvailableOnDemand $ loadResDataStartState expId (StartStateWarmUp key)
        ResultDataRep key -> AvailableOnDemand $ loadResDataStartState expId (StartStateRep key)
    mkEndStateAvailableOnDemand =
      case resData ^. resultDataKey of
        ResultDataPrep key -> AvailableOnDemand $ loadResDataEndState expId (EndStatePrep key)
        ResultDataWarmUp key -> AvailableOnDemand $ loadResDataEndState expId (EndStateWarmUp key)
        ResultDataRep key -> AvailableOnDemand $ loadResDataEndState expId (EndStateRep key)
    addInputValsAndMeasure inputVals measures resData =
      let countResults' = resData ^. results . _1 + length measures
          countInputValues' = resData ^. inputValues . _1 + length inputVals
       in case resData ^. resultDataKey of
            ResultDataPrep key -> do
              when (delInputs) $ deleteCascadeWhere [PrepInputPrepResultData ==. key, PrepInputPeriod >=. curLen + 1]
              inpKeys <- insertMany $ map (PrepInput key . view inputValuePeriod) inputVals
              insertMany_ $ zipWith (\k v -> PrepInputValue k (runPut . put . view inputValue $ v)) inpKeys inputVals
              measureKeys <- insertMany $ map (PrepMeasure key . view measurePeriod) measures
              insertMany_ $ concat $ zipWith (\k (Measure _ xs) -> map (\(StepResult n mX y) -> PrepResultStep k n mX y) xs) measureKeys measures
              return $ results .~ (countResults', AvailableOnDemand (loadPrepartionMeasures key)) $ inputValues .~
                (countInputValues', AvailableOnDemand (fromMaybe [] <$> loadPreparationInput key)) $
                resData
            ResultDataWarmUp key -> do
              when (delInputs) $ deleteCascadeWhere [WarmUpInputRepResult ==. key, WarmUpInputPeriod >=. curLen + 1]
              inpKeys <- insertMany $ map (WarmUpInput key . view inputValuePeriod) inputVals
              insertMany_ $ zipWith (\k v -> WarmUpInputValue k (runPut . put . view inputValue $ v)) inpKeys inputVals
              measureKeys <- insertMany $ map (WarmUpMeasure key . view measurePeriod) measures
              insertMany_ $ concat $ zipWith (\k (Measure _ xs) -> map (\(StepResult n mX y) -> WarmUpResultStep k n mX y) xs) measureKeys measures
              return $ results .~ (countResults', AvailableOnDemand (loadReplicationWarmUpMeasures key)) $ inputValues .~
                (countInputValues', AvailableOnDemand (fromMaybe [] <$> loadReplicationWarmUpInput key)) $
                resData
            ResultDataRep key -> do
              when (delInputs) $ deleteCascadeWhere [RepInputRepResult ==. key, RepInputPeriod >=. curLen + 1]
              inpKeys <- insertMany $ map (RepInput key . view inputValuePeriod) inputVals
              insertMany_ $ zipWith (\k v -> RepInputValue k (runPut . put . view inputValue $ v)) inpKeys inputVals
              measureKeys <- insertMany $ map (RepMeasure key . view measurePeriod) measures
              insertMany_ $ concat $ zipWith (\k (Measure _ xs) -> map (\(StepResult n mX y) -> RepResultStep k n mX y) xs) measureKeys measures
              return $ results .~ (countResults', AvailableOnDemand (loadReplicationMeasures key)) $ inputValues .~
                (countInputValues', AvailableOnDemand (fromMaybe [] <$> loadReplicationInput key)) $
                resData
    upd :: (ExperimentDef a) => RepResultType -> ResultData a -> ReaderT SqlBackend (LoggingT (ExpM a)) ()
    upd (Prep expResId) (ResultData (ResultDataPrep k) sTime eTime sG eG inpVals ress sSt eSt sInpSt eInpSt) = do
      sGBS <- liftIO $ fromRandGen sG
      eGBS <- liftIO $ maybe (return Nothing) (fmap Just . fromRandGen) eG
      update
        k
        [ PrepResultDataStartTime =. sTime
        , PrepResultDataEndTime =. eTime
        , PrepResultDataStartRandGen =. sGBS
        , PrepResultDataEndRandGen =. eGBS
        , PrepResultDataStartInputState =. runPut (put sInpSt)
        , PrepResultDataEndInputState =. runPut . put <$> eInpSt
        ]
      when (curLen == 0) $ do
        ~serSt <- mkTransientlyAvailable sSt >>= lift . lift . serialisable
        setResDataStartState (StartStatePrep k) (runPut $ put serSt)
      serESt <- mkTransientlyAvailable eSt >>= lift . lift . traverse serialisable
      setResDataEndState (EndStatePrep k) (runPut . put <$> serESt)
    upd (WarmUp repResId) (ResultData (ResultDataWarmUp k) sTime eTime sG eG inpVals ress sSt eSt sInpSt eInpSt) = do
      sGBS <- liftIO $ fromRandGen sG
      eGBS <- liftIO $ maybe (return Nothing) (fmap Just . fromRandGen) eG
      update
        k
        [ WarmUpResultDataStartTime =. sTime
        , WarmUpResultDataEndTime =. eTime
        , WarmUpResultDataStartRandGen =. sGBS
        , WarmUpResultDataEndRandGen =. eGBS
        , WarmUpResultDataStartInputState =. runPut (put sInpSt)
        , WarmUpResultDataEndInputState =. runPut . put <$> eInpSt
        ]
      when (curLen == 0) $ do
        ~serSt <- mkTransientlyAvailable sSt >>= lift . lift . serialisable
        setResDataStartState (StartStateWarmUp k) (runPut $ put serSt)
      serESt <- mkTransientlyAvailable eSt >>= lift . lift . traverse serialisable
      setResDataEndState (EndStateWarmUp k) (runPut . put <$> serESt)
    upd (Rep repResId) (ResultData (ResultDataRep k) sTime eTime sG eG inpVals ress sSt eSt sInpSt eInpSt) = do
      sGBS <- liftIO $ fromRandGen sG
      eGBS <- liftIO $ maybe (return Nothing) (fmap Just . fromRandGen) eG
      update
        k
        [ RepResultDataStartTime =. sTime
        , RepResultDataEndTime =. eTime
        , RepResultDataStartRandGen =. sGBS
        , RepResultDataEndRandGen =. eGBS
        , RepResultDataStartInputState =. runPut (put sInpSt)
        , RepResultDataEndInputState =. runPut . put <$> eInpSt
        ]
      when (curLen == 0) $ do
        ~serSt <- mkTransientlyAvailable sSt >>= lift . lift . serialisable
        setResDataStartState (StartStateRep k) (runPut $ put serSt)
      serESt <- mkTransientlyAvailable eSt >>= lift . lift . traverse serialisable
      setResDataEndState (EndStateRep k) (runPut . put <$> serESt)
    upd _ _ = error "Unexpected update combination. This is a bug, please report it!"


run :: (ExperimentDef a) => (GenIO, a, InputState a, [Input a], [Measure]) -> Int -> ReaderT SqlBackend (LoggingT (ExpM a)) (GenIO, a, InputState a, [Input a], [Measure])
run (g, st, stInp, inpVals, res) period = do
  -- let (randGen, g') = split g
  (inpVal', inpSt') <- lift $ lift $ generateInput g st stInp period
  (res', st') <- lift $ lift $ runStep st inpVal' period
  return (g, st', inpSt', Input period inpVal' : inpVals, Measure period res' : res)
