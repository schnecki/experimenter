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
import           Data.Serialize
import qualified Data.Text                   as T
import           Data.Time                   (getCurrentTime)
import           Database.Persist.Postgresql
import           System.Random

import           Experimenter.Experiment
import           Experimenter.Models
import           Experimenter.Parameter
import           Experimenter.Result
import           Experimenter.Setup
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
runExperimentResult exps expRes
  -- TODO prep
 = do
  repRes <- getRepRes exps (expRes ^. evaluationResults) >>= mapM (runReplicationResult exps)
  let updated = any fst repRes
      res = map snd repRes
  return (updated, set evaluationResults res expRes)
  where
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
    run rD = ((delNeeded ||) *** Just) <$> runResultData exps rD


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
    run rD = ((delNeeded ||) *** Just) <$> runResultData exps rD


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


runResultData :: (ExperimentDef a, MonadLogger m, MonadIO m) => Experiments a -> ResultData a -> ReaderT SqlBackend m (Updated, ResultData a)
runResultData exps resData = do
  undefined

