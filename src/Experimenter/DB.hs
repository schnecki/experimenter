{-# LANGUAGE OverloadedStrings #-}
module Experimenter.DB where


import           Conduit                      as C
import           Control.Monad.IO.Unlift      (MonadUnliftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader   (ReaderT)
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

