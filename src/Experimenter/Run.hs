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
import           Data.Maybe                  (fromMaybe)
import           Data.Time                   (getCurrentTime)
import           Database.Persist.Postgresql

import           Experimenter.Experiment
import           Experimenter.Models
import           Experimenter.Result
import           Experimenter.Setup


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
  let expRepetitions = exp ^. experimentSetup.expSetupRepetitions
      expInitSt = exp ^. experimentInitialState
      expInitInpSt = exp ^. experimentInitialInputState

  -- TODO: parallelisation
  new <- zipWithM (runExperimentResult expInitSt expInitInpSt) (fmap Just (exp ^. experimentResults) ++ repeat Nothing) [1..expRepetitions]
  endTime <- return <$> liftIO getCurrentTime
  update (exp ^. experimentKey) [ExpEndTime =. endTime]
  return $ set experimentResults new $ set experimentEndTime endTime exp

  -- let paramSetups = parameters initSt
  --     paramSetting = map (`mkParameterSetting` initSt) paramSetups

  --     params = map (convertParameterSetup kExp) paramSetups
  --     replicationsNeeded = setup ^. evaluationReplications
  -- expResults <- queryParamSettingsGrouped kExp

runExperimentResult :: (ExperimentDef a, MonadLogger m, MonadIO m) => a -> InputState a -> Maybe (ExperimentResult a) -> Int -> ReaderT SqlBackend m (ExperimentResult a)
runExperimentResult st inpSt mExpRes repNr = do

  undefined


