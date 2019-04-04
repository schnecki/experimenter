{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE Unsafe              #-}

module Experimenter.Run
    ( DatabaseSetup (..)
    , runExperiments
    ) where

import           Control.Arrow               (second, (***))
import           Control.Lens
import           Control.Monad               (forM)
import           Control.Monad.IO.Class
import           Control.Monad.Logger        (MonadLogger, runStderrLoggingT)
import           Control.Monad.Reader
import qualified Data.ByteString             as BS
import           Data.Maybe                  (fromMaybe, isNothing)
import           Data.Serialize              hiding (get)
import qualified Data.Text                   as T
import           Data.Time                   (getCurrentTime)
import           Database.Persist.Postgresql
import           System.Random

import           Experimenter.Experiment
import           Experimenter.Input
import           Experimenter.Measure
import           Experimenter.Models
import           Experimenter.Parameter
import           Experimenter.Result
import           Experimenter.Setup
import           Experimenter.StepResult
import           Experimenter.Util


data DatabaseSetup = DatabaseSetup
  { connectionString    :: BS.ByteString -- ^. e.g. "host=localhost dbname=experimenter user=postgres password=postgres port=5432"
  , parallelConnections :: Int
  }


type Updated = Bool


runExperiments :: forall a . (ExperimentDef a) => DatabaseSetup -> ExperimentSetup -> InputState a -> a -> IO (Bool, Experiments a)
runExperiments dbSetup setup initInpSt initSt =
  runStderrLoggingT $
  withPostgresqlPool (connectionString dbSetup) (parallelConnections dbSetup) $
  liftSqlPersistMPool $ do
    runMigration migrateAll
    loadExperiments setup initInpSt initSt >>= continueExperiments


continueExperiments :: (ExperimentDef a, MonadLogger m, MonadIO m) => Experiments a -> ReaderT SqlBackend m (Bool, Experiments a)
continueExperiments exp = do
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
      return [Experiment kExp 1 startTime Nothing (initParams exp) []]
    mkNewExps exp expsDone = error "modifying parameters are not yet implemented" undefined


continueExperiment :: (ExperimentDef a, MonadLogger m, MonadIO m) => Experiments a -> Experiment a  -> ReaderT SqlBackend m (Updated, Experiment a)
continueExperiment exps exp = do
  -- TODO: parallelisation
  expRes <- getExpRes exps (exp ^. experimentResults) >>= mapM (runExperimentResult exps)
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


newResultData :: a -> InputState a -> IO (ResultData a)
newResultData st inpSt = do
  time <- getCurrentTime
  g <- newStdGen
  return $ ResultData time Nothing g Nothing [] [] st Nothing inpSt Nothing


runExperimentResult :: (ExperimentDef a, MonadLogger m, MonadIO m) => Experiments a -> ExperimentResult a -> ReaderT SqlBackend m (Updated, ExperimentResult a)
runExperimentResult exps expRes = do
  (prepUpdated, prepRes) <- runPreparation exps expResId (expRes ^. preparationResults)
  repsDone <-
    if prepUpdated
      then do
        mapM_ deleteReplicationResult (expRes ^. evaluationResults)
        return []
      else return (expRes ^. evaluationResults)
  repRes <- getRepRes exps repsDone >>= mapM (runReplicationResult exps)
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
      then deleteResultData (Eval repResId) >> return Nothing
      else return (repRes ^. evalResults)
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
    else return (delNeeded, mResData')
  where
    delNeeded = maybe False (\r -> prepSteps < length (r ^. results)) mResData
    runNeeded = maybe (prepSteps > 0) (\r -> prepSteps > length (r ^. results) || (delNeeded && prepSteps > 0)) mResData
    prepSteps = exps ^. experimentsSetup . expsSetupPreparationSteps
    initSt = exps ^. experimentsInitialState
    initInpSt = exps ^. experimentsInitialInputState
    new = liftIO $ newResultData initSt initInpSt
    run rD = ((delNeeded ||) *** Just) <$> runResultData exps (Prep expResId) rD


runWarmUp :: (ExperimentDef a, MonadLogger m, MonadIO m) => Experiments a -> Key RepResult -> Maybe (ResultData a) -> ReaderT SqlBackend m (Updated, Maybe (ResultData a))
runWarmUp exps repResId mResData = do
  mResData' <-
    if delNeeded
      then deleteResultData (WarmUp repResId) >> return Nothing
      else return mResData
  if runNeeded
    then maybe new return mResData' >>= run
    else return (delNeeded, mResData')
  where
    delNeeded = maybe False (\r -> wmUpSteps < length (r ^. results)) mResData
    runNeeded = maybe (wmUpSteps > 0) (\r -> wmUpSteps > length (r ^. results) || (delNeeded && wmUpSteps > 0)) mResData
    wmUpSteps = exps ^. experimentsSetup . expsSetupEvaluationWarmUpSteps
    initSt = exps ^. experimentsInitialState
    initInpSt = exps ^. experimentsInitialInputState
    new = liftIO $ newResultData initSt initInpSt
    run rD = ((delNeeded ||) *** Just) <$> runResultData exps (WarmUp repResId) rD


runEval :: (ExperimentDef a, MonadLogger m, MonadIO m) => Experiments a -> Updated -> Key RepResult -> Maybe (ResultData a) -> ReaderT SqlBackend m (Updated, Maybe (ResultData a))
runEval exps warmUpUpdated repResId mResData = do
  mResData' <-
    if delNeeded
      then deleteResultData (Eval repResId) >> return Nothing
      else return mResData
  if runNeeded
    then maybe new return mResData' >>= run
    else return (delNeeded, mResData')
  where
    delNeeded = warmUpUpdated || maybe False (\r -> evalSteps < length (r ^. results)) mResData
    runNeeded = maybe (evalSteps > 0) (\r -> evalSteps > length (r ^. results)) mResData
    evalSteps = exps ^. experimentsSetup . expsSetupEvaluationSteps
    initSt = exps ^. experimentsInitialState
    initInpSt = exps ^. experimentsInitialInputState
    new = liftIO $ newResultData initSt initInpSt
    run rD = ((delNeeded ||) *** Just) <$> runResultData exps (Eval repResId) rD


deleteReplicationResult :: (MonadIO m) => ReplicationResult a -> ReaderT SqlBackend m ()
deleteReplicationResult (ReplicationResult repResId _ _ _) = deleteCascade repResId


data RepResultType
  = Prep (Key ExpResult)
  | WarmUp (Key RepResult)
  | Eval (Key RepResult)


deleteResultData :: (MonadLogger m, MonadIO m) => RepResultType -> ReaderT SqlBackend m ()
deleteResultData repResType =
  case repResType of
    Prep expResId   -> del (UniquePrepResultDataExpResult expResId)
    WarmUp repResId -> del (UniqueWarmUpResultDataRepResult repResId)
    Eval repResId   -> del (UniqueRepResultDataRepResult repResId)
  where
    del unique = getBy unique >>= \x -> sequence_ (fmap (deleteCascade . entityKey)x)


runResultData :: (ExperimentDef a, MonadLogger m, MonadIO m) => Experiments a -> RepResultType -> ResultData a -> ReaderT SqlBackend m (Updated, ResultData a)
runResultData exps repResType resData = do
  let len = undefined
  let isNew = null (resData ^. results)
  let st = fromMaybe (resData ^. startState) (resData ^. endState)
  let stInp = fromMaybe (resData ^. startInputState) (resData ^. endInputState)
  let g = fromMaybe (resData ^. startRandGen) (resData ^. endRandGen)
  let periodsToRun = [1 + length (resData ^. results) .. len]
  let updated = not (null periodsToRun)
  (g', st', stInp', inputs, measures) <- foldM run (g, st, stInp, [], []) periodsToRun
  if updated
    then do
    eTime <- pure <$> liftIO getCurrentTime
    let resData' = set endTime eTime resData

    ins repResType resData'
    return (True, resData')
    else return (False, resData)
  where
    run :: (RandomGen g, ExperimentDef a, MonadLogger m, MonadIO m)
      => (g, a, InputState a, [Input a], [Measure]) -> Int -> ReaderT SqlBackend m (g, a, InputState a, [Input a], [Measure])
    run (g, st, stInp, inpVals, res) period = do
      let (randGen, g') = split g
      (inpVal', inpSt') <- lift $ generateInput randGen st stInp period
      (res', st') <- lift $ runStep st inpVal' period
      return (g', st', inpSt', Input period inpVal' : inpVals, Measure period res' : res)

    ins (Prep expResId) (ResultData sTime eTime sG eG inpVals ress sSt eSt sInpSt eInpSt) =
      undefined
