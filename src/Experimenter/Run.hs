{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Unsafe              #-}

module Experimenter.Run
    ( DatabaseSetup (..)
    , runExperiments
    ) where

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
  repRes <- getRepRes exps (expRes ^. evaluationResults) >>= mapM (runResultData exps)
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

  (wmUpChange, wmUp) <- getResultData exps (exps ^. experimentsInitialState) (exps ^. experimentsInitialInputState) (repRes ^. warmUpResults) >>= runResultData exps -- warm up
  let mEnd = fromMaybe (error "Warm up phase finished with unfinished state")
  (evalChange, eval) <- getResultData exps (mEnd $ wmUp ^. endState) (mEnd $ wmUp ^. endInputState) (repRes ^. evalResults) >>= runResultData exps -- warm up
  undefined

  where getResultData :: (MonadIO m) => Experiments a -> a -> InputState a -> Maybe (ResultData a) -> ReaderT SqlBackend m (ResultData a)
        getResultData exps st stInp Nothing = liftIO $ newResultData (exps ^. experimentsInitialState) (exps ^. experimentsInitialInputState)
        getResultData exps st stInp (Just x) | delete eval if warm-up length increased, etc.
          = return $ set startState st $ set startInputState stInp x


runResultData :: (ExperimentDef a, MonadLogger m, MonadIO m) => Experiments a -> ResultData a -> ReaderT SqlBackend m (Updated, ResultData a)
runResultData exps resData = do
  undefined


  -- let expKey = exp ^. experimentKey
  --     expNrReplics = exp ^. experimentSetup . expSetupEvaluationReplications
  --     expInitSt = exp ^. experimentInitialState
  --     expInitInpSt = exp ^. experimentInitialInputState
  --     mPrepSteps = exp ^. experimentSetup . expSetupPreparationSteps
  -- expRes <- case eiExpRes of
  --   Right x | isNothing (x ^. preparationResults) && maybe False (>0) mPrepSteps -> (\v -> set preparationResults (Just v) x) <$> liftIO (newResultData expInitSt expInitInpSt)
  --   Right x -> return x
  --   Left paramSetting -> do
  --     prepRes <- liftIO $ newResultData expInitSt expInitInpSt
  --     key <- insert $ ExpResult expKey repetitionNr
  --     return $ ExperimentResult key repetitionNr paramSetting (Just prepRes) []
  -- let replications = expRes ^. evaluationResults
  -- if length replications >= expNrReplics
  --   then return expRes
  --   else do
  -- let definedRandGens = map (startRandGen) replications
  -- randomGens <- (definedRandGens++) <$> liftIO (replicateM () newStdGen)
  -- let mkReplicationResult nr = do
  --       g <- liftIO newStdGen
  --       key <- insert $ RepResult (expRes ^. experimentResultKey) nr (tshow g) Nothing Nothing Nothing Nothing Nothing Nothing
  --       return $ ReplicationResult key nr g
  -- startReplications <- (replications ++) <$> mapM mkReplicationResult [1+length replications..expNrReplics]
  -- new <- zipWithM (runReplicationResult exp) startReplications [1..expNrReplics]
  -- undefined

-- mkPrepStartInputValuesAndRand :: Experiment a -> (InputState a, StdGen)


-- runReplicationResult :: (ExperimentDef a, MonadLogger m, MonadIO m) => Experiment a -> ReplicationResult a -> Int -> ReaderT SqlBackend m (ReplicationResult a)
-- runReplicationResult exp randGen repNr = do

--   undefined
