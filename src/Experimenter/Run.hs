{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Experimenter.Run
    ( DatabaseSetup (..)
    , execExperiments
    , runExperiments
    , runExperimentsIO
    ) where

import           Control.Arrow                (first, (***))
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
import           Data.Maybe                   (fromMaybe, isJust, isNothing)
import           Data.Pool                    as P
import           Data.Serialize               hiding (get)
import qualified Data.Serialize               as S
import qualified Data.Text                    as T
import           Data.Time                    (getCurrentTime)
import qualified Database.Esqueleto           as E
import           Database.Persist.Postgresql
import           System.IO
import           System.Random
import           System.Random.Shuffle

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


data DatabaseSetup = DatabaseSetup
  { connectionString    :: BS.ByteString -- ^. e.g. "host=localhost dbname=experimenter user=postgres password=postgres port=5432"
  , parallelConnections :: Int
  }


type Updated = Bool

execExperiments :: (ExperimentDef a) => (ExpM a (Bool, Experiments a) -> IO (Bool, Experiments a)) -> DatabaseSetup -> ExperimentSetup -> InputState a -> a -> IO (Experiments a)
execExperiments runExpM dbSetup setup initInpSt initSt = snd <$> runExperiments runExpM dbSetup setup initInpSt initSt

runExperiments :: (ExperimentDef a) => (ExpM a (Bool, Experiments a) -> IO (Bool, Experiments a)) -> DatabaseSetup -> ExperimentSetup -> InputState a -> a -> IO (Bool, Experiments a)
runExperiments = runner

runExperimentsIO :: (ExperimentDef a, IO ~ ExpM a) => DatabaseSetup -> ExperimentSetup -> InputState a -> a -> IO (Bool, Experiments a)
runExperimentsIO = runner id


runner :: (ExperimentDef a) => (ExpM a (Bool, Experiments a) -> IO (Bool, Experiments a)) -> DatabaseSetup -> ExperimentSetup -> InputState a -> a -> IO (Bool, Experiments a)
runner runExpM dbSetup setup initInpSt initSt = do
  runStdoutLoggingT $ withPostgresqlPool (connectionString dbSetup) (parallelConnections dbSetup) $ liftSqlPersistMPool $
    runMigration migrateAll
  runExpM $ (runStdoutLoggingT  . filterLogger (\s _ -> s /= "SQL")) $ withPostgresqlConn (connectionString dbSetup) $ \backend ->
    flip runSqlConn backend $ loadExperiments setup initInpSt initSt >>= checkUniqueParamNames >>= runExperiment

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
  $(logDebug) $ "Processing experiment with ID " <> tshow (unSqlBackendKey $ unExpsKey $ exp ^. experimentsKey)
  liftIO $ hFlush stdout
  let exps = exp ^. experiments
  rands <- liftIO $ mkRands exps
  newExps <- mkNewExps exp exps
  let expsList = exps ++ newExps
  $(logDebug) $ "Number of experiments loaded: " <> tshow (length exps)
  $(logDebug) $ "Number of new experiments: " <> tshow (length newExps)
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
    mkParamSetting :: Experiments a -> ParameterSetup a -> ParameterSetting a
    mkParamSetting exp (ParameterSetup name setter getter mod bnds drp) = ParameterSetting name (runPut $ put $ getter (exp ^. experimentsInitialState)) (drp $ getter (exp ^. experimentsInitialState))
    initParams exp = map (mkParamSetting exp) (view experimentsParameters exp)
    mkNewExps :: (ExperimentDef a) => Experiments a -> [Experiment a] -> ReaderT SqlBackend (LoggingT (ExpM a)) [Experiment a]
    mkNewExps exp [] = do
      $(logDebug) $ "Initializing new experiments..."
      startTime <- liftIO getCurrentTime
      kExp <- insert $ Exp (exp ^. experimentsKey) 1 startTime Nothing
      saveParamSettings kExp (initParams exp)
      return [Experiment kExp 1 startTime Nothing (initParams exp) []]
    mkNewExps exp expsDone = do
      $(logDebug) "Checking whether adding further experiments is necessary..."
      params <- liftIO $ shuffleM $ parameters (exp ^. experimentsInitialState)
      if null params
        then return []
        else do
          startTime <- liftIO getCurrentTime
          let p = head params
          let otherParams = map (`mkParameterSetting` st) (tail params)
          paramSettings <- modifyParam exp p
          paramSettings' <- filterParamSettings exp p otherParams paramSettings
          let settings = map (\x -> L.sortBy (compare `on` view parameterSettingName) (x : otherParams)) paramSettings'
          let nrs = [1 + length expsDone .. length expsDone + length settings]
          kExps <- mapM (\nr -> insert $ Exp (exp ^. experimentsKey) nr startTime Nothing) nrs
          zipWithM_ saveParamSettings kExps settings
          let exps = zipWith3 (\key nr params -> Experiment key nr startTime Nothing params []) kExps nrs settings
          unless (null exps) $ $(logDebug) $ "Created " <> tshow (length exps) <> " new experiments variations!"
          return exps
      where
        st = exp ^. experimentsInitialState
    filterParamSettings exp p otherParams paramSettings = do
      expIds <- map entityKey <$> selectList [ExpExps ==. exp ^. experimentsKey] []
      pExists <-
        map (map (view paramSettingExp . entityVal)) <$>
        mapM (\v -> selectList [ParamSettingExp <-. expIds, ParamSettingName ==. parameterName p, ParamSettingValue ==. view parameterSettingValue v] []) paramSettings
      paramSettingExits <-
        map or <$>
        mapM
          (\pValsIds ->
             mapM
               (\k -> do
                  ps <- map entityVal <$> selectList [ParamSettingExp ==. k] []
                  return $ all (\(ParameterSetting n vBs drp) -> maybe False ((vBs ==) . view paramSettingValue) $ L.find ((== n) . view paramSettingName) ps) otherParams)
               (L.nub pValsIds))
          pExists
      return $ map fst $ filter (not . snd) $ zip paramSettings paramSettingExits
    modifyParam :: (MonadIO m) => Experiments a -> ParameterSetup a -> ReaderT SqlBackend m [ParameterSetting a]
    modifyParam exps (ParameterSetup _ _ _ Nothing _ _) = return []
    modifyParam exps (ParameterSetup n setter getter (Just modifier) mBounds drp) = do
      pairs <-
        E.select $ E.from $ \(exp, par) -> do
          E.where_ (exp E.^. ExpExps E.==. E.val (view experimentsKey exps))
          E.where_ (par E.^. ParamSettingExp E.==. exp E.^. ExpId)
          return (exp, par)
      let concatRight Left {}   = []
          concatRight (Right x) = [x]
      let vals = concatMap (concatRight . S.runGet S.get . view paramSettingValue . entityVal . snd) pairs
      let filterBounds x = case mBounds of
            Nothing           -> True
            Just (minB, maxB) -> x <= maxB && x >= minB
      bss <- liftIO $ concat <$> mapM (\val -> zip (repeat val) . fmap (runPut . put) . filter filterBounds <$> modifier (getter $ setter val st)) vals
      return $ map (\(v, bs) -> ParameterSetting n bs (drp v)) bss
      where
        st = exps ^. experimentsInitialState
    saveParamSettings kExp = mapM_ (\(ParameterSetting n bs drp) -> insert $ ParamSetting kExp n bs drp)
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


deleteExperiment :: (MonadIO m) => Experiment a -> ReaderT SqlBackend m ()
deleteExperiment (Experiment k _ _ _ _ expRes) = mapM_ deleteExperimentResult expRes >> deleteCascade k


deleteExperimentResult :: (MonadIO m) => ExperimentResult a -> ReaderT SqlBackend m ()
deleteExperimentResult (ExperimentResult k _ _ repls) = mapM_ deleteReplicationResult repls >> deleteCascade k

-- | Loads parameters of an experiment into all initial and end states of the given experiments variable.
loadParameters  :: (ExperimentDef a) => Experiments a -> Experiment a  -> ReaderT SqlBackend (LoggingT (ExpM a)) (Experiments a)
loadParameters exps exp = foldM setParam exps (exp ^. parameterSetup)
  where
    setParam e (ParameterSetting n bs drp) =
      case L.find (\(ParameterSetup name _ _ _ _ _) -> name == n) parameterSetups of
        Nothing -> do
          $(logDebug) $ "Could not find parameter with name " <> n <> " in the current parameter setting. Thus it will not be modified!"
          return e
        Just (ParameterSetup _ setter _ _ _ drp) ->
          case runGet S.get bs of
            Left err -> error $ "Could not read value of parameter " <> T.unpack n <> ". Aborting! Serializtion error was: " ++ err
            Right val -> do
              $(logDebug) $ "Loaded parameter '" <> n <> "' value: " <> tshow val
              return $ foldl' (\e stSet -> over stSet (setter val) e) e
                [ experimentsInitialState
                , experiments.traversed.experimentResults.traversed.preparationResults.traversed.startState
                , experiments.traversed.experimentResults.traversed.preparationResults.traversed.endState.traversed
                , experiments.traversed.experimentResults.traversed.evaluationResults.traversed.warmUpResults.traversed.startState
                , experiments.traversed.experimentResults.traversed.evaluationResults.traversed.warmUpResults.traversed.endState.traversed
                , experiments.traversed.experimentResults.traversed.evaluationResults.traversed.evalResults.traversed.startState
                , experiments.traversed.experimentResults.traversed.evaluationResults.traversed.evalResults.traversed.endState.traversed
                ]
    parameterSetups = parameters (exps ^. experimentsInitialState)


type Rands = ([StdGen],[StdGen],[StdGen]) -- ^ Preparation, Warm Up and Evaluation random generators


continueExperiment :: (ExperimentDef a) => Rands -> Experiments a -> Experiment a  -> ReaderT SqlBackend (LoggingT (ExpM a)) (Updated, Experiment a)
continueExperiment rands exps exp = do
  -- TODO: parallelisation
  exps' <- loadParameters exps exp -- loads parameters into the init state
  expResList <- getExpRes exps' (exp ^. experimentResults) >>= truncateExperiments repetits
  $(logDebug) $ "Number of experiment results loaded: " <> tshow (length expResList)
  let dropPrep = or (map (^. parameterDropPrepeationPhase) (exp ^. parameterSetup) )
  expRes <- mapM (runExperimentResult dropPrep rands exps') expResList
  let updated = any fst expRes
      res = map snd expRes
  if updated
    then do endTime <- return <$> liftIO getCurrentTime
            update (exp ^. experimentKey) [ExpEndTime =. endTime]
            return (updated, set experimentResults res $ set experimentEndTime endTime exp)
    else return (updated, set experimentResults res exp)

  where
    repetits = exps ^. experimentsSetup . expsSetupRepetitions
    getExpRes :: (MonadIO m) => Experiments a -> [ExperimentResult a] -> ReaderT SqlBackend m [ExperimentResult a]
    getExpRes exps expResDone =
      (expResDone ++) <$>
      forM
        [length expResDone + 1 .. repetits]
        (\nr -> do
           startTime <- liftIO getCurrentTime
           kExpRes <- insert $ ExpResult (exp ^. experimentKey) nr
           return $ ExperimentResult kExpRes nr Nothing [])

    truncateExperiments nr xs = do
      let dels = drop nr xs
      unless (null dels) $ $(logDebug) $ "Number of experiment repetitions being deleted " <> tshow (length dels)
      mapM_ deleteExperimentResult dels
      unless (null dels) transactionSave
      return $ take nr xs


newResultData :: (ExperimentDef a, Serialize (InputState a), MonadIO m) => StdGen -> RepResultType -> a -> InputState a -> ReaderT SqlBackend m (ResultData a)
newResultData g repResType st inpSt = do
  time <- liftIO getCurrentTime
  k <- case repResType of
          Prep expResId   -> ResultDataPrep <$> insert (PrepResultData expResId time Nothing (tshow g) Nothing (runPut $ put $ serialisable st) Nothing (runPut $ put inpSt) Nothing)
          WarmUp repResId -> ResultDataWarmUp <$> insert (WarmUpResultData repResId time Nothing (tshow g) Nothing  (runPut $ put $ serialisable st) Nothing (runPut $ put inpSt) Nothing)
          Rep repResId    -> ResultDataRep <$> insert (RepResultData repResId time Nothing (tshow g) Nothing (runPut $ put $ serialisable st) Nothing (runPut $ put inpSt) Nothing)
  return $ ResultData k time Nothing g Nothing [] [] st Nothing inpSt Nothing


type DropPreparation = Bool

runExperimentResult :: (ExperimentDef a) => DropPreparation -> Rands -> Experiments a -> ExperimentResult a -> ReaderT SqlBackend (LoggingT (ExpM a)) (Updated, ExperimentResult a)
runExperimentResult dropPrep rands@(prepRands,_,_) exps expRes = do

  (prepUpdated, prepRes) <- runPreparation (prepRands !! (expRes^.repetitionNumber-1)) exps expResId (expRes ^. preparationResults)
  repsDone <-
    if prepUpdated
      then do
        mapM_ deleteReplicationResult (expRes ^. evaluationResults)
        return []
      else return (expRes ^. evaluationResults)
  transactionSave
  let initSt = fromMaybe (exps ^. experimentsInitialState) (join $ fmap (view endState) prepRes)
      initInpSt = fromMaybe (exps ^. experimentsInitialInputState) (join $ fmap (view endInputState) prepRes)
  let runRepl e repRess = do
        res <- runReplicationResult rands e (expRes^.repetitionNumber) initSt initInpSt repRess
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
      $(logDebug) $ "Number of loaded replications: " <> tshow (length repsDone)
      $(logDebug) $ "Number of new replications: " <> tshow (exps ^. experimentsSetup . expsSetupEvaluationReplications - length repsDone)
      (repsDone ++) <$> forM
        [length repsDone + 1 .. exps ^. experimentsSetup . expsSetupEvaluationReplications]
        (\nr -> do
           kRepRes <- insert $ RepResult (expRes ^. experimentResultKey) nr
           return $ ReplicationResult kRepRes nr Nothing Nothing)


runPreparation :: (ExperimentDef a) => StdGen -> Experiments a -> Key ExpResult -> Maybe (ResultData a) -> ReaderT SqlBackend (LoggingT (ExpM a)) (Updated, Maybe (ResultData a))
runPreparation g exps expResId mResData = do
  mResData' <-
    if delNeeded
      then deleteResultData (Prep expResId) >> return Nothing
      else return mResData
  when delNeeded $ $(logDebug) "Updating Preparation (deletion of data was required)"
  when runNeeded $ $(logDebug) "Updating Preparation (a run was required)"
  if runNeeded
    then maybe new return mResData' >>= run
    else return (delNeeded || runNeeded, mResData')
  where
    delNeeded = maybe False (\r -> prepSteps < length (r ^. results)) mResData
    runNeeded = maybe (prepSteps > 0) (\r -> prepSteps > length (r ^. results) || (delNeeded && prepSteps > 0)) mResData
    prepSteps = exps ^. experimentsSetup . expsSetupPreparationSteps
    initSt = exps ^. experimentsInitialState
    initInpSt = exps ^. experimentsInitialInputState
    new = newResultData g (Prep expResId) initSt initInpSt
    run rD = ((delNeeded ||) *** Just) <$> runResultData prepSteps (Prep expResId) rD


type RepetitionNr = Int

type InitialState a = a
type InitialInputState a = InputState a

runReplicationResult ::
     (ExperimentDef a)
  => Rands
  -> Experiments a
  -> RepetitionNr
  -> InitialState a
  -> InitialInputState a
  -> ReplicationResult a
  -> ReaderT SqlBackend (LoggingT (ExpM a)) (Updated, ReplicationResult a)
runReplicationResult (_, wmUpRands, replRands) exps repetNr initSt initInpSt repRes = do
  (wmUpChange, mWmUp) <- runWarmUp (wmUpRands !! ((repetNr - 1) * replicats + (repRes ^. replicationNumber - 1))) exps (repRes ^. replicationResultKey) initSt initInpSt (repRes ^. warmUpResults)
  let initStEval = fromMaybe initSt (join $ fmap (view endState) mWmUp)
      initInpStEval = fromMaybe initInpSt (join $ fmap (view endInputState) mWmUp)
  mEval <-
    if wmUpChange
      then deleteResultData (Rep repResId) >> return Nothing
      else return (repRes ^. evalResults)
  when wmUpChange $ $(logDebug) $ "A change in the settings of the warm up phase occurred. Discarding the result data."
  (evalChange, mEval') <- runEval (replRands !! ((repetNr - 1) * replicats + (repRes ^. replicationNumber - 1))) exps wmUpChange (repRes ^. replicationResultKey) initStEval initInpStEval mEval
  return (wmUpChange || evalChange, set warmUpResults mWmUp $ set evalResults mEval' repRes)
  where
    repResId = repRes ^. replicationResultKey
    replicats = exps ^. experimentsSetup . expsSetupEvaluationReplications


runWarmUp ::
     (ExperimentDef a)
  => StdGen
  -> Experiments a
  -> Key RepResult
  -> InitialState a
  -> InitialInputState a
  -> Maybe (ResultData a)
  -> ReaderT SqlBackend (LoggingT (ExpM a)) (Updated, Maybe (ResultData a))
runWarmUp g exps repResId initSt initInpSt mResData = do
  mResData' <-
    if delNeeded
      then deleteResultData (WarmUp repResId) >> return Nothing
      else return mResData
  if runNeeded
    then maybe new return mResData' >>= run
    else do
      when delNeeded $ $(logDebug) "Updating WarmUp (delNeeded)"
      return (delNeeded, mResData')
  where
    delNeeded = maybe False (\r -> wmUpSteps < length (r ^. results)) mResData
    runNeeded = maybe (wmUpSteps > 0) (\r -> wmUpSteps > length (r ^. results) || (delNeeded && wmUpSteps > 0)) mResData
    wmUpSteps = exps ^. experimentsSetup . expsSetupEvaluationWarmUpSteps
    new = newResultData g (WarmUp repResId) initSt initInpSt
    run rD = ((delNeeded ||) *** Just) <$> runResultData wmUpSteps (WarmUp repResId) rD


runEval ::
     (ExperimentDef a)
  => StdGen
  -> Experiments a
  -> Updated
  -> Key RepResult
  -> InitialState a
  -> InitialInputState a
  -> Maybe (ResultData a)
  -> ReaderT SqlBackend (LoggingT (ExpM a)) (Updated, Maybe (ResultData a))
runEval g exps warmUpUpdated repResId initSt initInpSt mResData = do
  $(logDebug) "Starting evaluation..."
  mResData' <-
    if delNeeded
      then deleteResultData (Rep repResId) >> return Nothing
      else return mResData
  if runNeeded
    then do
      $(logDebug) $ "A run is needed for replication with ID " <> tshow (unSqlBackendKey $ unRepResultKey repResId)
      maybe new return mResData' >>= run
    else return (delNeeded, mResData')
  where
    delNeeded = warmUpUpdated || maybe False (\r -> evalSteps < length (r ^. results)) mResData
    runNeeded = maybe (evalSteps > 0) (\r -> evalSteps > length (r ^. results)) mResData
    evalSteps = exps ^. experimentsSetup . expsSetupEvaluationSteps
    new = newResultData g (Rep repResId) initSt initInpSt
    run rD = ((delNeeded ||) *** Just) <$> runResultData evalSteps (Rep repResId) rD


deleteReplicationResult :: (MonadIO m) => ReplicationResult a -> ReaderT SqlBackend m ()
deleteReplicationResult (ReplicationResult repResId _ _ _) = deleteCascade repResId


data RepResultType
  = Prep (Key ExpResult)
  | WarmUp (Key RepResult)
  | Rep (Key RepResult)


deleteResultData :: (MonadIO m) => RepResultType -> ReaderT SqlBackend m ()
deleteResultData repResType =
  case repResType of
    Prep expResId   -> do
      del (UniquePrepResultDataExpResult expResId)
      selectList [PrepInputExpResult ==. expResId] [] >>= mapM_ (deleteCascade . entityKey)
      selectList [PrepMeasureExpResult ==. expResId] [] >>= mapM_ (deleteCascade . entityKey)
    WarmUp repResId -> do
      del (UniqueWarmUpResultDataRepResult repResId)
      selectList [WarmUpInputRepResult ==. repResId] [] >>= mapM_ (deleteCascade . entityKey)
      selectList [WarmUpMeasureRepResult ==. repResId] [] >>= mapM_ (deleteCascade . entityKey)
    Rep repResId    -> do
      del (UniqueRepResultDataRepResult repResId)
      selectList [RepInputRepResult ==. repResId] [] >>= mapM_ (deleteCascade . entityKey)
      selectList [RepMeasureRepResult ==. repResId] [] >>= mapM_ (deleteCascade . entityKey)
  where
    del unique = getBy unique >>= mapM_ (deleteCascade . entityKey)


runResultData :: (ExperimentDef a) => Int -> RepResultType -> ResultData a -> ReaderT SqlBackend (LoggingT (ExpM a)) (Updated, ResultData a)
runResultData len repResType resData = do
  let isNew = null (resData ^. results)
  let st = fromMaybe (resData ^. startState) (resData ^. endState)
  let stInp = fromMaybe (resData ^. startInputState) (resData ^. endInputState)
  let g = fromMaybe (resData ^. startRandGen) (resData ^. endRandGen)
  let periodsToRun = [1 + length (resData ^. results) .. len]
  $(logDebug) $ "Number of periods to run: " <> tshow (length periodsToRun)
  let updated = not (null periodsToRun)
  sTime <- liftIO getCurrentTime
  (g', st', stInp', inputs, measures) <- foldM run (g, st, stInp, [], []) periodsToRun
  if updated
    then do
      $(logDebug) "Updating Result Data"
      eTime <- pure <$> liftIO getCurrentTime
      let resData' =
            (if isNew
               then set startTime sTime
               else id) $
            set results measures $ set inputValues inputs $ set endInputState (Just stInp') $ set endState (Just st') $ set endRandGen (Just g') $ set endTime eTime resData
      upd repResType resData'
      return (True, resData')
    else return (False, resData)
  where
    upd (Prep expResId) (ResultData (ResultDataPrep k) sTime eTime sG eG inpVals ress sSt eSt sInpSt eInpSt) = do
      inpKeys <- mapM (insert . PrepInput expResId . view inputValuePeriod) inpVals
      zipWithM_ (\k v -> insert $ PrepInputValue k (runPut . put . view inputValue $ v)) inpKeys inpVals
      measureKeys <- mapM (insert . PrepMeasure expResId . view measurePeriod) ress
      zipWithM_ (\k (Measure _ xs) -> mapM (\(StepResult n mX y) -> insert $ PrepResultStep k n mX y) xs) measureKeys ress
      replace k (PrepResultData expResId sTime eTime (tshow sG) (tshow <$> eG) (runPut . put $ serialisable sSt) (runPut . put . serialisable <$> eSt) (runPut . put $ sInpSt) (runPut . put <$> eInpSt))
    upd (WarmUp repResId) (ResultData (ResultDataWarmUp k) sTime eTime sG eG inpVals ress sSt eSt sInpSt eInpSt) = do
      inpKeys <- mapM (insert . WarmUpInput repResId . view inputValuePeriod) inpVals
      zipWithM_ (\k v -> insert $ WarmUpInputValue k (runPut . put . view inputValue $ v)) inpKeys inpVals
      measureKeys <- mapM (insert . WarmUpMeasure repResId . view measurePeriod) ress
      zipWithM_ (\k (Measure _ xs) -> mapM (\(StepResult n mX y) -> insert $ WarmUpResultStep k n mX y) xs) measureKeys ress
      replace k (WarmUpResultData repResId sTime eTime (tshow sG) (tshow <$> eG) (runPut . put $ serialisable sSt) (runPut . put . serialisable <$> eSt) (runPut . put $ sInpSt) (runPut . put <$> eInpSt))
    upd (Rep repResId) (ResultData (ResultDataRep k) sTime eTime sG eG inpVals ress sSt eSt sInpSt eInpSt) = do
      inpKeys <- mapM (insert . RepInput repResId . view inputValuePeriod) inpVals
      zipWithM_ (\k v -> insert $ RepInputValue k (runPut . put . view inputValue $ v)) inpKeys inpVals
      measureKeys <- mapM (insert . RepMeasure repResId . view measurePeriod) ress
      zipWithM_ (\k (Measure _ xs) -> mapM (\(StepResult n mX y) -> insert $ RepResultStep k n mX y) xs) measureKeys ress
      replace k (RepResultData repResId sTime eTime (tshow sG) (tshow <$> eG) (runPut . put $ serialisable sSt) (runPut . put . serialisable <$> eSt) (runPut . put $ sInpSt) (runPut . put <$> eInpSt))
    upd _ _ = error "Unexpected update combination. This is a bug, please report it!"


run :: (ExperimentDef a)
  => (StdGen, a, InputState a, [Input a], [Measure])
  -> Int
  -> ReaderT SqlBackend (LoggingT (ExpM a)) (StdGen, a, InputState a, [Input a], [Measure])
run (g, st, stInp, inpVals, res) period = do
  let (randGen, g') = split g
  (inpVal', inpSt') <- lift $ lift $ generateInput randGen st stInp period
  (res', st') <- lift $ lift $ runStep st inpVal' period
  return (g', st', inpSt', Input period inpVal' : inpVals, Measure period res' : res)
