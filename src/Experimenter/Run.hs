{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Experimenter.Run
    ( runExperiment
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
  { connectionString    :: BS.ByteString
  , parallelConnections :: Int
  }


runExperiment :: forall a . (ExperimentDef a) => DatabaseSetup -> ExperimentSetup -> InputState a -> a -> IO ()
runExperiment dbSetup setup initInpSt initSt =
  runStderrLoggingT $
  withPostgresqlPool (connectionString dbSetup) (parallelConnections dbSetup) $
  liftSqlPersistMPool $ do
    runMigration migrateAll
    time <- liftIO getCurrentTime
    let name = (setup ^. experimentBaseName)
    exps <- selectList [ExpName ==. name] []
    params <- mapM (\e -> selectList [ParamExperiment ==. entityKey e] []) exps
    let mkParamTpl (Param _ n minB maxB) = (n,minB,maxB)
    let mkMyParams :: [ParameterSetup a] -> [(T.Text, BS.ByteString, BS.ByteString)]
        mkMyParams xs = map (mkParamTpl . convertParameterSetup (error "Ref not used")) xs
        myParams :: [(T.Text, BS.ByteString, BS.ByteString)]
        myParams =  mkMyParams (parameters initSt)

          -- map (convertParameterSetup (error "Ref not used")) (parameters initSt)

    -- case find ((== myParams) . map (entityVal)) params of
    --   Nothing -> do
    --     kExp <- insert $ Exp name time Nothing (runPut $ put initSt) (runPut $ put initInpSt)
    liftIO $ print time

getExperiments :: MonadIO m => ReaderT SqlBackend m [Entity Exp]
getExperiments = rawSql "select ?? from person where name=?" [PersistText "sibi"]


convertParameterSetup :: ExpId -> ParameterSetup a -> Param
convertParameterSetup expId param = Param expId (param ^. parameterName) (runPut $ put $ param ^. bounds._1) (runPut $ put $ param ^. bounds._2)
