{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Experimenter.Eval.Csv
    ( writeCsvMeasureIO
    , writeCsvMeasure
    , Smoothing (..)
    , MeasureName
    ) where

import           Control.Arrow                ((&&&), (***))
import           Control.Lens                 hiding (Cons, Over, over)
import           Control.Monad                (unless)
import           Control.Monad.Logger         (logDebug)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Function                (on)
import           Data.List                    (find, sortBy)
import           Data.Maybe                   (fromMaybe)
import qualified Data.Text                    as T
import           Database.Persist.Postgresql  (SqlBackend, runSqlConn, withPostgresqlConn)

import           Experimenter.DatabaseSetting
import           Experimenter.Eval.Reduce
import           Experimenter.Eval.Type       as E
import           Experimenter.Experiment
import           Experimenter.Measure
import           Experimenter.Result.Type
import           Experimenter.StepResult
import           Experimenter.Util


data Smoothing = NoSmoothing | MovAvg Int

type MeasureName = T.Text

writeCsvMeasureIO :: (ExperimentDef a, ExpM a ~ IO) => DatabaseSetting -> Experiments a -> Smoothing -> [MeasureName] -> IO ()
writeCsvMeasureIO = writeCsvMeasure id


writeCsvMeasure :: (ExperimentDef a) => (ExpM a () -> IO ()) -> DatabaseSetting -> Experiments a -> Smoothing -> [MeasureName] -> IO ()
writeCsvMeasure runExpM dbSetup exps smoothing measures = runner runExpM dbSetup exps smoothing measures

runner :: (ExperimentDef a) => (ExpM a () -> IO ()) -> DatabaseSetting -> Experiments a -> Smoothing -> [MeasureName] -> IO ()
runner runExpM dbSetup exps smoothing measures =
  runExpM $
  (runStdoutLoggingT . filterLogger (\s _ -> s /= "SQL")) $ withPostgresqlConn (connectionString dbSetup) $ \backend -> flip runSqlConn backend $
  mapM_ (loadReduceAndWriteFile exps smoothing) measures

loadReduceAndWriteFile :: (ExperimentDef a) => Experiments a -> Smoothing -> MeasureName -> ReaderT SqlBackend (LoggingT (ExpM a)) ()
loadReduceAndWriteFile = do

  undefined
