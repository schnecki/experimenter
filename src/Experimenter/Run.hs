{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Experimenter.Run
    ( DatabaseSetup (..)
    , runExperiments
    , runExperimentsLogging
    , runExperimentsLoggingNoSql
    ) where

import           Control.Arrow                ((***))
import           Control.Lens
import           Control.Monad                (forM)
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger         (LogLevel (..), LoggingT, MonadLogger,
                                               defaultLoc, filterLogger, logDebug,
                                               logError, logInfo, runFileLoggingT,
                                               runLoggingT, runNoLoggingT,
                                               runStderrLoggingT, runStdoutLoggingT)

import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as BS
import           Data.Function                (on)
import qualified Data.List                    as L
import           Data.Maybe                   (fromMaybe, isJust)
import           Data.Pool                    as P
import           Data.Serialize               hiding (get)
import qualified Data.Serialize               as S
import qualified Data.Text                    as T
import           Data.Time                    (getCurrentTime)
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


runExperiments :: (ExperimentDef a) => DatabaseSetup -> ExperimentSetup -> InputState a -> a -> IO (Bool, Experiments a)
runExperiments = runner runNoLoggingT runNoLoggingT

runExperimentsLogging :: (ExperimentDef a) => DatabaseSetup -> ExperimentSetup -> InputState a -> a -> IO (Bool, Experiments a)
runExperimentsLogging = runner runStdoutLoggingT runStdoutLoggingT

runExperimentsLoggingNoSql :: (ExperimentDef a) => DatabaseSetup -> ExperimentSetup -> InputState a -> a -> IO (Bool, Experiments a)
runExperimentsLoggingNoSql = runner (runStdoutLoggingT . filterLogger (\s _ -> s /= "SQL")) runStdoutLoggingT


runSqlPersistMPoolLogging :: (IsSqlBackend backend, MonadUnliftIO m, MonadUnliftIO m1) => (m a -> ResourceT m1 a1) -> ReaderT backend m a -> Pool backend -> m1 a1
runSqlPersistMPoolLogging logFun x pool = runResourceT $ logFun $ runSqlPool x pool

liftSqlPersistMPoolLogging ::
     (IsPersistBackend backend, MonadUnliftIO m1, MonadIO m2, BaseBackend backend ~ SqlBackend) => (m1 a1 -> ResourceT IO a2) -> ReaderT backend m1 a1 -> Pool backend -> m2 a2
liftSqlPersistMPoolLogging logFun x pool = liftIO (runSqlPersistMPoolLogging logFun x pool)

runner ::
     (MonadUnliftIO m, MonadUnliftIO m1, ExperimentDef a1, MonadLogger m, MonadLogger m1)
  => (m a2 -> t)
  -> (m1 (Bool, Experiments a1) -> ResourceT IO a2)
  -> DatabaseSetup
  -> ExperimentSetup
  -> InputState a1
  -> a1
  -> t
runner logFunDb logFunRun dbSetup setup initInpSt initSt =
  logFunDb $
  withPostgresqlPool (connectionString dbSetup) (parallelConnections dbSetup) $
  liftSqlPersistMPoolLogging logFunRun $ do
    runMigration migrateAll
    loadExperiments setup initInpSt initSt >>= continueExperiments


continueExperiments :: (ExperimentDef a, MonadLogger m, MonadIO m) => Experiments a -> ReaderT SqlBackend m (Bool, Experiments a)
continueExperiments exp = do
  $(logInfo) $ "Processing experiment with ID " <> tshow (exp ^. experimentsKey)
  liftIO $ hFlush stdout
  let exps = exp ^. experiments
  newExps <- mkNewExps exp exps
  expRes <- mapM (continueExperiment exp) (exps ++ newExps)
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
    mkParamSetting exp (ParameterSetup name setter getter mod bnds) = ParameterSetting name (runPut $ put $ getter (exp ^. experimentsInitialState))
    initParams exp = map (mkParamSetting exp) (view experimentsParameters exp)
    mkNewExps :: (ExperimentDef a, MonadIO m) => Experiments a -> [Experiment a] -> ReaderT SqlBackend m [Experiment a]
    mkNewExps exp [] = do
      startTime <- liftIO getCurrentTime
      kExp <- insert $ Exp (exp ^. experimentsKey) 1 startTime Nothing
      saveParamSettings kExp (initParams exp)
      return [Experiment kExp 1 startTime Nothing (initParams exp) []]
    mkNewExps exp expsDone = do
      params <- liftIO $ shuffleM $ parameters (exp ^. experimentsInitialState)
      if null params
        then return []
        else do
          startTime <- liftIO getCurrentTime
          paramSettings <- liftIO $ modifyParam exp (head params)
          let otherParams = map (`mkParameterSetting` st) (tail params)
          let settings = map (\x -> L.sortBy (compare `on` view parameterSettingName) (x:otherParams)) paramSettings
          let nrs = [1+length expsDone..length expsDone+length settings]
          kExps <- mapM (\nr -> insert $ Exp (exp ^. experimentsKey) nr startTime Nothing) nrs
          zipWithM_ saveParamSettings kExps settings
          let exps = zipWith3 (\key nr params -> Experiment key nr startTime Nothing params []) kExps nrs settings
          return exps
      where
        st = exp ^. experimentsInitialState

    modifyParam :: Experiments a -> ParameterSetup a -> IO [ParameterSetting a]
    modifyParam exp (ParameterSetup _ _ _ Nothing _) = return []
    modifyParam exp (ParameterSetup n setter getter (Just modifier) (minBound, maxBound)) = do
      bss <- fmap (runPut . put) . filter (\x -> x <= maxBound && x >= minBound) <$> modifier (getter st)
      return $ map (ParameterSetting n) bss
      where
        st = exp ^. experimentsInitialState

    saveParamSettings kExp = mapM_ (\(ParameterSetting n bs) -> insert $ ParamSetting kExp n bs)


loadParameters  :: (ExperimentDef a, MonadLogger m) => Experiments a -> Experiment a  -> ReaderT SqlBackend m (Experiments a)
loadParameters exps exp = foldM setParam exps (exp ^. parameterSetup)
  where
    setParam e (ParameterSetting n bs) =
      case L.find (\(ParameterSetup name _ _ _ _) -> name == n) parameterSetups of
        Nothing -> do
          $(logError) $ "Could not find parameter with name " <> n <> " in the current parameter setting. Thus it will not be modified!"
          return e
        Just (ParameterSetup _ setter _ _ _) ->
          case runGet S.get bs of
            Left err -> error $ "Could not read value of parameter " <> T.unpack n <> ". Aborting! Serializtion error was: " ++ err
            Right val -> do
              let st' = setter val (e ^. experimentsInitialState)
              return $ set experimentsInitialState st' e

    parameterSetups = parameters (exps ^. experimentsInitialState)


continueExperiment :: (ExperimentDef a, MonadLogger m, MonadIO m) => Experiments a -> Experiment a  -> ReaderT SqlBackend m (Updated, Experiment a)
continueExperiment exps exp = do
  -- TODO: parallelisation
  exps' <- loadParameters exps exp
  expRes <- getExpRes exps' (exp ^. experimentResults) >>= mapM (runExperimentResult exps')
  let updated = any fst expRes
      res = map snd expRes
  if updated
    then do endTime <- return <$> liftIO getCurrentTime
            update (exp ^. experimentKey) [ExpEndTime =. endTime]
            return (updated, set experimentResults res $ set experimentEndTime endTime exp)
    else return (updated, set experimentResults res exp)

  where
    getExpRes :: (MonadIO m) => Experiments a -> [ExperimentResult a] -> ReaderT SqlBackend m [ExperimentResult a]
    getExpRes exps expResDone =
      (expResDone ++) <$>
      forM
        [length expResDone + 1 .. exps ^. experimentsSetup . expsSetupRepetitions]
        (\nr -> do
           startTime <- liftIO getCurrentTime
           kExpRes <- insert $ ExpResult (exp ^. experimentKey) nr
           return $ ExperimentResult kExpRes nr Nothing [])


newResultData :: (Serialize a, Serialize (InputState a), MonadIO m) => RepResultType -> a -> InputState a -> ReaderT SqlBackend m (ResultData a)
newResultData repResType st inpSt = do
  time <- liftIO getCurrentTime
  g <- liftIO newStdGen
  k <- case repResType of
          Prep expResId   -> ResultDataPrep <$> insert (PrepResultData expResId time Nothing (tshow g) Nothing (runPut $ put st) Nothing (runPut $ put inpSt) Nothing)
          WarmUp repResId -> ResultDataWarmUp <$> insert (WarmUpResultData repResId time Nothing (tshow g) Nothing  (runPut $ put st) Nothing (runPut $ put inpSt) Nothing)
          Rep repResId    -> ResultDataRep <$> insert (RepResultData repResId time Nothing (tshow g) Nothing (runPut $ put st) Nothing (runPut $ put inpSt) Nothing)
  return $ ResultData k time Nothing g Nothing [] [] st Nothing inpSt Nothing


runExperimentResult :: (ExperimentDef a, MonadLogger m, MonadIO m) => Experiments a -> ExperimentResult a -> ReaderT SqlBackend m (Updated, ExperimentResult a)
runExperimentResult exps expRes = do
  (prepUpdated, prepRes) <- runPreparation exps expResId (expRes ^. preparationResults)
  repsDone <-
    if prepUpdated
      then do
        mapM_ deleteReplicationResult (expRes ^. evaluationResults)
        return []
      else return (expRes ^. evaluationResults)
  transactionSave
  let runRepl e repRess = do
        res <- runReplicationResult e repRess
        transactionSave
        return res
  repRes <- getRepRes exps repsDone >>= mapM (runRepl exps)
  let updated = any fst repRes
      res = map snd repRes
  return (prepUpdated || updated, set preparationResults prepRes $ set evaluationResults res expRes)
  where
    expResId = expRes ^. experimentResultKey
    getRepRes :: (MonadIO m) => Experiments a -> [ReplicationResult a] -> ReaderT SqlBackend m [ReplicationResult a]
    getRepRes exps repsDone =
      (repsDone ++) <$>
      forM
        [length repsDone + 1 .. exps ^. experimentsSetup . expsSetupEvaluationReplications]
        (\nr -> do
           kRepRes <- insert $ RepResult (expRes ^. experimentResultKey) nr
           return $ ReplicationResult kRepRes nr Nothing Nothing)


runReplicationResult :: (ExperimentDef a, MonadLogger m, MonadIO m) => Experiments a -> ReplicationResult a -> ReaderT SqlBackend m (Updated, ReplicationResult a)
runReplicationResult exps repRes = do
  (wmUpChange, mWmUp) <- runWarmUp exps (repRes ^. replicationResultKey) (repRes ^. warmUpResults)
  mEval <-
    if wmUpChange
      then deleteResultData (Rep repResId) >> return Nothing
      else return (repRes ^. evalResults)
  $(logInfo) $ "WarmUp Change: " <> tshow wmUpChange
  (evalChange, mEval') <- runEval exps wmUpChange (repRes ^. replicationResultKey) mEval
  return (wmUpChange || evalChange, set warmUpResults mWmUp $ set evalResults mEval' repRes)
  where
    repResId = repRes ^. replicationResultKey


runPreparation :: (ExperimentDef a, MonadLogger m, MonadIO m) => Experiments a -> Key ExpResult -> Maybe (ResultData a) -> ReaderT SqlBackend m (Updated, Maybe (ResultData a))
runPreparation exps expResId mResData = do
  mResData' <-
    if delNeeded
      then deleteResultData (Prep expResId) >> return Nothing
      else return mResData
  if runNeeded
    then maybe new return mResData' >>= run
    else do
    when delNeeded $ $(logDebug) "Updating Preparation (delNeeded)"
    return (delNeeded, mResData')
  where
    delNeeded = maybe False (\r -> prepSteps < length (r ^. results)) mResData
    runNeeded = maybe (prepSteps > 0) (\r -> prepSteps > length (r ^. results) || (delNeeded && prepSteps > 0)) mResData
    prepSteps = exps ^. experimentsSetup . expsSetupPreparationSteps
    initSt = exps ^. experimentsInitialState
    initInpSt = exps ^. experimentsInitialInputState
    new = newResultData (Prep expResId) initSt initInpSt
    run rD = ((delNeeded ||) *** Just) <$> runResultData prepSteps (Prep expResId) rD


runWarmUp :: (ExperimentDef a, MonadLogger m, MonadIO m) => Experiments a -> Key RepResult -> Maybe (ResultData a) -> ReaderT SqlBackend m (Updated, Maybe (ResultData a))
runWarmUp exps repResId mResData = do
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
    initSt = exps ^. experimentsInitialState
    initInpSt = exps ^. experimentsInitialInputState
    new = newResultData (WarmUp repResId) initSt initInpSt
    run rD = ((delNeeded ||) *** Just) <$> runResultData wmUpSteps (WarmUp repResId) rD


runEval :: (ExperimentDef a, MonadLogger m, MonadIO m) => Experiments a -> Updated -> Key RepResult -> Maybe (ResultData a) -> ReaderT SqlBackend m (Updated, Maybe (ResultData a))
runEval exps warmUpUpdated repResId mResData = do
  mResData' <-
    if delNeeded
      then deleteResultData (Rep repResId) >> return Nothing
      else return mResData
  if runNeeded
    then do
    $(logInfo) $ "DelNeeded: " <> tshow delNeeded
    $(logInfo) $  "A run is needed for replication " <> tshow repResId
    $(logInfo) $ "mResData': " <> tshow (isJust mResData')
    maybe new return mResData' >>= run
    else do
    when delNeeded $ $(logDebug) "Updating Eval (delNeeded)"
    return (delNeeded, mResData')
  where
    delNeeded = warmUpUpdated || maybe False (\r -> evalSteps < length (r ^. results)) mResData
    runNeeded = maybe (evalSteps > 0) (\r -> evalSteps > length (r ^. results)) mResData
    evalSteps = exps ^. experimentsSetup . expsSetupEvaluationSteps
    initSt = exps ^. experimentsInitialState
    initInpSt = exps ^. experimentsInitialInputState
    new = newResultData (Rep repResId) initSt initInpSt
    run rD = ((delNeeded ||) *** Just) <$> runResultData evalSteps (Rep repResId) rD


deleteReplicationResult :: (MonadIO m) => ReplicationResult a -> ReaderT SqlBackend m ()
deleteReplicationResult (ReplicationResult repResId _ _ _) = deleteCascade repResId


data RepResultType
  = Prep (Key ExpResult)
  | WarmUp (Key RepResult)
  | Rep (Key RepResult)


deleteResultData :: (MonadLogger m, MonadIO m) => RepResultType -> ReaderT SqlBackend m ()
deleteResultData repResType =
  case repResType of
    Prep expResId   -> del (UniquePrepResultDataExpResult expResId)
    WarmUp repResId -> del (UniqueWarmUpResultDataRepResult repResId)
    Rep repResId    -> del (UniqueRepResultDataRepResult repResId)
  where
    del unique = getBy unique >>= \x -> sequence_ (fmap (deleteCascade . entityKey)x)

runResultData :: (ExperimentDef a, MonadLogger m, MonadIO m) => Int -> RepResultType -> ResultData a -> ReaderT SqlBackend m (Updated, ResultData a)
runResultData len repResType resData = do
  let isNew = null (resData ^. results)
  let st = fromMaybe (resData ^. startState) (resData ^. endState)
  let stInp = fromMaybe (resData ^. startInputState) (resData ^. endInputState)
  let g = fromMaybe (resData ^. startRandGen) (resData ^. endRandGen)
  let periodsToRun = [1 + length (resData ^. results) .. len]
  $(logDebug) $ "PeriodsToRun : " <> tshow periodsToRun
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
      replace k (PrepResultData expResId sTime eTime (tshow sG) (tshow <$> eG) (runPut . put $ sSt) (runPut . put <$> eSt) (runPut . put $ sInpSt) (runPut . put <$> eInpSt))
    upd (WarmUp repResId) (ResultData (ResultDataWarmUp k) sTime eTime sG eG inpVals ress sSt eSt sInpSt eInpSt) = do
      inpKeys <- mapM (insert . WarmUpInput repResId . view inputValuePeriod) inpVals
      zipWithM_ (\k v -> insert $ WarmUpInputValue k (runPut . put . view inputValue $ v)) inpKeys inpVals
      measureKeys <- mapM (insert . WarmUpMeasure repResId . view measurePeriod) ress
      zipWithM_ (\k (Measure _ xs) -> mapM (\(StepResult n mX y) -> insert $ WarmUpResultStep k n mX y) xs) measureKeys ress
      replace k (WarmUpResultData repResId sTime eTime (tshow sG) (tshow <$> eG) (runPut . put $ sSt) (runPut . put <$> eSt) (runPut . put $ sInpSt) (runPut . put <$> eInpSt))
    upd (Rep repResId) (ResultData (ResultDataRep k) sTime eTime sG eG inpVals ress sSt eSt sInpSt eInpSt) = do
      inpKeys <- mapM (insert . RepInput repResId . view inputValuePeriod) inpVals
      zipWithM_ (\k v -> insert $ RepInputValue k (runPut . put . view inputValue $ v)) inpKeys inpVals
      measureKeys <- mapM (insert . RepMeasure repResId . view measurePeriod) ress
      zipWithM_ (\k (Measure _ xs) -> mapM (\(StepResult n mX y) -> insert $ RepResultStep k n mX y) xs) measureKeys ress
      replace k (RepResultData repResId sTime eTime (tshow sG) (tshow <$> eG) (runPut . put $ sSt) (runPut . put <$> eSt) (runPut . put $ sInpSt) (runPut . put <$> eInpSt))
    upd _ _ = error "Unexpected update combination. This is a bug, please report it!"


run :: (RandomGen g, ExperimentDef a, MonadLogger m)
  => (g, a, InputState a, [Input a], [Measure])
  -> Int
  -> ReaderT SqlBackend m (g, a, InputState a, [Input a], [Measure])
run (g, st, stInp, inpVals, res) period = do
  let (randGen, g') = split g
  (inpVal', inpSt') <- lift $ generateInput randGen st stInp period
  (res', st') <- lift $ runStep st inpVal' period
  return (g', st', inpSt', Input period inpVal' : inpVals, Measure period res' : res)
