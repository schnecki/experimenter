{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Unsafe              #-}

module Experimenter.Run
    ( DatabaseSetup (..)
    , runExperiment
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger        (MonadLogger, runStderrLoggingT)
import           Control.Monad.Reader
import qualified Data.ByteString             as BS
import           Database.Persist.Postgresql

import           Experimenter.Experiment
import           Experimenter.Models
import           Experimenter.Result
import           Experimenter.Setup


data DatabaseSetup = DatabaseSetup
  { connectionString    :: BS.ByteString -- ^. e.g. "host=localhost dbname=experimenter user=postgres password=postgres port=5432"
  , parallelConnections :: Int
  }


runExperiment :: forall a . (ExperimentDef a) => DatabaseSetup -> ExperimentSetup -> InputState a -> a -> IO ()
runExperiment dbSetup setup initInpSt initSt =
  runStderrLoggingT $
  withPostgresqlPool (connectionString dbSetup) (parallelConnections dbSetup) $
  liftSqlPersistMPool $ do
    runMigration migrateAll
    loadExperiment setup initInpSt initSt >>= continueExperiment


continueExperiment :: (ExperimentDef a, MonadLogger m, MonadIO m) => Experiment a -> ReaderT SqlBackend m ()
continueExperiment exp = do
  -- let paramSetups = parameters initSt
  --     paramSetting = map (`mkParameterSetting` initSt) paramSetups

  --     params = map (convertParameterSetup kExp) paramSetups
  --     replicationsNeeded = setup ^. evaluationReplications
  -- expResults <- queryParamSettingsGrouped kExp


  undefined


