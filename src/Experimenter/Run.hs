{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Unsafe              #-}

module Experimenter.Run
    ( DatabaseSetup (..)
    , runExperiments
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


runExperiments :: forall a . (ExperimentDef a) => DatabaseSetup -> ExperimentSetup -> InputState a -> a -> IO (Experiments a)
runExperiments dbSetup setup initInpSt initSt =
  runStderrLoggingT $
  withPostgresqlPool (connectionString dbSetup) (parallelConnections dbSetup) $
  liftSqlPersistMPool $ do
    runMigration migrateAll
    loadExperiments setup initInpSt initSt >>= continueExperiments


continueExperiments :: (ExperimentDef a, MonadLogger m, MonadIO m) => Experiments a -> ReaderT SqlBackend m (Experiments a)
continueExperiments exp = do
  undefined

continueExperiment :: (ExperimentDef a, MonadLogger m, MonadIO m) => Experiments a -> Int -> ReaderT SqlBackend m (Experiment a)
continueExperiment exp = do

  let expRepetitions = exp ^. experimentsSetup . expsSetupRepetitions
      expInitSt = exp ^. experimentsInitialState
      expInitInpSt = exp ^. experimentsInitialInputState
      preExpReps = exp ^. experimentsResults
  -- TODO: parallelisation
  new <- zipWithM (runExperimentResult exp) (fmap Right (exp ^. experimentResults) ++ repeat (Left $ error "TODO: parameter setting") ) [1 .. expRepetitions]
  if length preExpReps < length new
    then do
      endTime <- return <$> liftIO getCurrentTime
      update (exp ^. experimentsKey) [ExpEndTime =. endTime]
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
     (ExperimentDef a, MonadLogger m, MonadIO m) => Experiments a -> Either [ParameterSetting a] (ExperimentResult a) -> Int -> ReaderT SqlBackend m (ExperimentResult a)
runExperimentResult exp eiExpRes repetitionNr = do
  let expKey = exp ^. experimentKey
      expNrReplics = exp ^. experimentSetup . expSetupEvaluationReplications
      expInitSt = exp ^. experimentInitialState
      expInitInpSt = exp ^. experimentInitialInputState
      mPrepSteps = exp ^. experimentSetup . expSetupPreparationSteps
  expRes <- case eiExpRes of
    Right x | isNothing (x ^. preparationResults) && maybe False (>0) mPrepSteps -> (\v -> set preparationResults (Just v) x) <$> liftIO (newResultData expInitSt expInitInpSt)
    Right x -> return x
    Left paramSetting -> do
      prepRes <- liftIO $ newResultData expInitSt expInitInpSt
      key <- insert $ ExpResult expKey repetitionNr
      return $ ExperimentResult key repetitionNr paramSetting (Just prepRes) []

  let replications = expRes ^. evaluationResults

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
  undefined

-- mkPrepStartInputValuesAndRand :: Experiment a -> (InputState a, StdGen)


runReplicationResult :: (ExperimentDef a, MonadLogger m, MonadIO m) => Experiment a -> ReplicationResult a -> Int -> ReaderT SqlBackend m (ReplicationResult a)
runReplicationResult exp randGen repNr = do

  undefined
