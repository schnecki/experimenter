{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Experimenter.DB where


import           Conduit                      as C
import           Control.Monad.IO.Unlift      (MonadUnliftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader   (ReaderT)
import qualified Database.Esqueleto           as E
import           Database.Persist.Postgresql  (SqlBackend, withPostgresqlPool)

import           Experimenter.DatabaseSetting
import           Experimenter.Experiment

type DB a b = ReaderT SqlBackend (LoggingT (ExpM a)) b
type SimpleDB = ReaderT SqlBackend (LoggingT IO)


logFun :: (MonadIO m) => LoggingT m a -> m a
logFun = runStdoutLoggingT . filterLogger (\s _ -> s /= "SQL")

-- logFun :: NoLoggingT m a -> m a
-- logFun = runNoLoggingT


-- runDB :: (MonadLogger m, MonadUnliftIO m) => ConnectionString -> ReaderT SqlBackend (LoggingT (ResourceT m)) a -> m a
runDB :: (MonadUnliftIO m) => DatabaseSetting -> ReaderT SqlBackend (LoggingT m) a -> m a
runDB = runDBWithM id

runDBWithM :: (MonadUnliftIO m1) => (m1 a -> m a) -> DatabaseSetting -> ReaderT SqlBackend (LoggingT m1) a -> m a
runDBWithM runM dbSetting action = runM $ logFun $ withPostgresqlPool (connectionString dbSetting) (parallelConnections dbSetting) $ \pool -> E.runSqlPool action pool


-- withPostgresqlPool :: (MonadLogger m, MonadUnliftIO m) => ConnectionString -> Int -> (Pool SqlBackend -> m a) -> m a
-- runSqlPool :: (MonadUnliftIO m, BackendCompatible SqlBackend backend) => ReaderT backend m a -> Pool backend -> m a

