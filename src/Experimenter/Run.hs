{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Unsafe              #-}

module Experimenter.Run
    ( DatabaseSetup (..)
    , runExperiment
    ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Logger        (MonadLogger, runStderrLoggingT)
import           Control.Monad.Reader
import qualified Data.ByteString             as BS
import           Data.Maybe                  (fromMaybe, isNothing)
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


runExperiment :: forall a . (ExperimentDef a) => DatabaseSetup -> ExperimentSetup -> InputState a -> a -> IO (Experiment a)
runExperiment dbSetup setup initInpSt initSt =
  runStderrLoggingT $
  withPostgresqlPool (connectionString dbSetup) (parallelConnections dbSetup) $
  liftSqlPersistMPool $ do
    runMigration migrateAll
    loadExperiment setup initInpSt initSt >>= continueExperiment

-- TODO: modify parameters

continueExperiment :: (ExperimentDef a, MonadLogger m, MonadIO m) => Experiment a -> ReaderT SqlBackend m (Experiment a)
continueExperiment exp = do
  let expRepetitions = exp ^. experimentSetup . expSetupRepetitions
      expInitSt = exp ^. experimentInitialState
      expInitInpSt = exp ^. experimentInitialInputState
      preExpReps = exp ^. experimentResults
  -- TODO: parallelisation
  new <- zipWithM (runExperimentResult exp) (fmap Right (exp ^. experimentResults) ++ repeat (Left $ error "TODO: parameter setting") ) [1 .. expRepetitions]
  if length preExpReps < length new
    then do
      endTime <- return <$> liftIO getCurrentTime
      update (exp ^. experimentKey) [ExpEndTime =. endTime]
      return $ set experimentResults new $ set experimentEndTime endTime exp
    else return $ set experimentResults new exp
  -- let paramSetups = parameters initSt
  --     paramSetting = map (`mkParameterSetting` initSt) paramSetups
  --     params = map (convertParameterSetup kExp) paramSetups
  --     replicationsNeeded = setup ^. evaluationReplications
  -- expResults <- queryParamSettingsGrouped kExp

newResultData :: a -> InputState a -> IO (ResultData a)
newResultData st inpSt = do
  time <- getCurrentTime
  g <- newStdGen
  return $ ResultData time Nothing g Nothing [] [] st Nothing inpSt Nothing

runExperimentResult ::
     (ExperimentDef a, MonadLogger m, MonadIO m) => Experiment a -> Either [ParameterSetting a] (ExperimentResult a) -> Int -> ReaderT SqlBackend m (ExperimentResult a)
runExperimentResult exp eiExpRes repNr = do
  let expKey = exp ^. experimentKey
      expNrReplics = exp ^. experimentSetup . expSetupEvaluationReplications
      expInitSt = exp ^. experimentInitialState
      expInitInpSt = exp ^. experimentInitialInputState
  expRes <- case eiExpRes of
    Right x | isNothing (x ^. preparationResults) -> (\v -> set preparationResults (Just v) x) <$> liftIO (newResultData expInitSt expInitInpSt)
    Right x -> return x
    Left paramSetting -> do
      prepRes <- liftIO $ newResultData expInitSt expInitInpSt
      key <- insert $ ExpResult expKey repNr
      return $ ExperimentResult key repNr paramSetting (Just prepRes) []

  let replications = expRes ^. evaluationResults

  -- if length replications >= expNrReplics
  --   then return expRes
  --   else do
  let definedRandGens = map (view replicationRandomGenerator) replications
  -- randomGens <- (definedRandGens++) <$> liftIO (replicateM () newStdGen)
  let mkReplicationResult nr = do
        g <- liftIO newStdGen
        key <- insert $ RepResult (expRes ^. experimentResultKey) nr (tshow g) Nothing Nothing Nothing Nothing Nothing Nothing
        return $ ReplicationResult key nr g

  startReplications <- (replications ++) <$> mapM mkReplicationResult [1+length replications..expNrReplics]


  new <- zipWithM (runReplicationResult exp) startReplications [1..expNrReplics]
  undefined


runReplicationResult ::
     (ExperimentDef a, RandomGen g, MonadLogger m, MonadIO m) => Experiment a -> ReplicationResult a -> Int -> ReaderT SqlBackend m (ReplicationResult a)
runReplicationResult exp randGen repNr = do

  undefined
