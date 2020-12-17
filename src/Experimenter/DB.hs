{-# LANGUAGE OverloadedStrings #-}
module Experimenter.DB where


import           Conduit                      as C
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader   (ReaderT)
import           Data.Char
import           Data.List                    (foldl')
import qualified Data.Text                    as T
import qualified Database.Esqueleto           as E
import           Database.Persist.Postgresql  (SqlBackend, withPostgresqlPool)

import           Experimenter.DatabaseSetting


type DB m = ReaderT SqlBackend (LoggingT (ResourceT m))
type SimpleDB = DB IO

logFun :: (MonadIO m) => LoggingT m a -> m a
logFun = runStdoutLoggingT . filterLogger (\s _ -> s /= "SQL")

-- logFun :: NoLoggingT m a -> m a
-- logFun = runNoLoggingT


runDB :: (MonadUnliftIO m) => DatabaseSetting -> DB m a -> m a
runDB = runDBWithM runResourceT

runDBSimple :: DatabaseSetting -> SimpleDB a -> IO a
runDBSimple = runDBWithM runResourceT


runDBWithM :: (MonadUnliftIO m1) => (m1 a -> m a) -> DatabaseSetting -> ReaderT SqlBackend (LoggingT m1) a -> m a
runDBWithM runM dbSetting action = runM $ logFun $ withPostgresqlPool (connectionString dbSetting) (parallelConnections dbSetting) $ \pool -> E.runSqlPool action pool


-- withPostgresqlPool :: (MonadLogger m, MonadUnliftIO m) => ConnectionString -> Int -> (Pool SqlBackend -> m a) -> m a
-- runSqlPool :: (MonadUnliftIO m, BackendCompatible SqlBackend backend) => ReaderT backend m a -> Pool backend -> m a

-- | Create indices for fast lookups in the DB.
indexCreation :: (MonadIO m) => ReaderT SqlBackend (NoLoggingT (ResourceT m)) ()
indexCreation = mapM_ ((\x -> E.rawExecute ("CREATE INDEX IF NOT EXISTS " <> mkName x <> " ON " <> x) []) . mkLowerCase) indices
  where
    mkName txt = foldl' (\acc (from, to) -> T.replace from to acc) (txt <> "index") replacements
    replacements = [("(", "_"), (")", "_"), (",", "_"), ("\"", "")]
    mkLowerCase x = toLowerCase ( T.takeWhile (/= '(') x) <> T.singleton '(' <> toLowerCase (T.tail $ T.dropWhile (/= '(') x)
    toLowerCase =
        let go c
                | isUpper c = T.pack ['_', toLower c]
                | otherwise = T.singleton c
        in T.dropWhile (== '_') . T.concatMap go
    indices =
      [ -- "ExpsInfoParam(exps)"
      -- , "ExpsMaster(exps)"
      -- , "ExpsSetup(exps)"
      -- , "Param(exps)"
      -- , "Exp(exps)"
      -- , "ExpExecutionLock(exp)"
      -- , "ExpProgress(exp)"
      -- , "ParamSetting(exp)"
      -- , "ExpResult(exp)"
      -- , "PrepStartStatePart(resultData)"
      -- , "PrepEndStatePart(resultData)"
      -- , "PrepInput(prepResultData)"
      -- , "PrepInputValue(prepInput)"
      -- , "PrepMeasure(prepResultData)"
      -- , "PrepResultStep(measure)"
      -- , "RepResult(expResult)"

        "RepResultStep(measure)"
      , "WarmUpResultStep(measure)"
      , "PrepResultStep(measure)"

      , "RepMeasure(repResult)"
      , "WarmUpMeasure(repResult)"
      , "PrepMeasure(prepResultData)"

      , "RepInputValue(repInput)"
      , "WarmUpInputValue(warmUpInput)"
      , "PrepInputValue(prepInput)"

      , "RepStartStatePart(resultData)"
      , "RepEndStatePart(resultData)"


      -- , "\"user\"(ident)" -- user is a postgres keyword!
      ]
