{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Experimenter.Run
    ( DatabaseSetup (..)
    , runExperiment
    ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Logger        (LoggingT, runStderrLoggingT)
import           Control.Monad.Reader
import qualified Data.ByteString             as BS
import           Data.List                   (find)
import           Data.Serialize              (Serialize, put)
import           Data.Serialize.Put          (runPut)
import qualified Data.Text                   as T
import           Data.Time                   (getCurrentTime)
import qualified Database.Esqueleto          as E
import           Database.Persist
import           Database.Persist.Postgresql
import           Unsafe.Coerce

import           Experimenter.Experiment
import           Experimenter.Models
import           Experimenter.Parameter
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
    time <- liftIO getCurrentTime
    let name = setup ^. experimentBaseName
    exps <- selectList [ExpName ==. name] []
    params <- mapM (\e -> selectList [ParamExperiment ==. entityKey e] []) exps
    let mkParamTpl (Param _ n minB maxB) = (n, minB, maxB)
    let myParams = map (mkParamTpl . convertParameterSetup (error "Ref not used")) (parameters initSt)
    kExp <-
      case find ((== myParams) . map (mkParamTpl . entityVal) . snd) (zip exps params) of
        Nothing -> do
          liftIO $ putStrLn "Starting new experiment..."
          insert $ Exp name time Nothing (runPut $ put initSt) (runPut $ put initInpSt)
        Just (eExp, _) -> liftIO (putStrLn "Continuing experiment ...") >> return (entityKey eExp)
    continueExperiment kExp
    -- liftIO $ print time


continueExperiment :: MonadIO m => Key Exp -> ReaderT SqlBackend m ()
continueExperiment kExp = do

  undefined


convertParameterSetup :: ExpId -> ParameterSetup a -> Param
convertParameterSetup expId (ParameterSetup name _ _ _ (minB,maxB)) = Param expId name (runPut $ put minB) (runPut $ put maxB)
