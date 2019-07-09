{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Experimenter.MasterSlave
    ( WorkerStatus (..)
    , createKeepAliveFork
    , waitForSlaves
    , keepAliveTimeout
    ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Logger         (LoggingT, MonadLogger, NoLoggingT,
                                               WriterLoggingT, defaultLoc, filterLogger,
                                               logDebug, logError, logInfo,
                                               runFileLoggingT, runLoggingT, runNoLoggingT,
                                               runStderrLoggingT, runStdoutLoggingT)
import           Data.IORef

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import qualified Data.Text                    as T
import           Data.Time                    (UTCTime, diffUTCTime, getCurrentTime)
import           Database.Persist.Postgresql
import           Network.HostName
import           System.Posix.Process
import           System.Process


import           Experimenter.DatabaseSetup
import           Experimenter.Experiment
import           Experimenter.Models
import           Experimenter.Result
import           Experimenter.Util


keepAliveTimeout :: Num t => t
keepAliveTimeout = 10


data WorkerStatus = Working | Finished
  deriving (Eq, Show)

createKeepAliveFork :: DatabaseSetup -> (UTCTime -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()) -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) () -> IO (IORef WorkerStatus)
createKeepAliveFork dbSetup updateFunction deletionFunction = do
  ref <- liftIO $ newIORef Working
  void $ liftIO $ forkIO (runNoLoggingT $ withPostgresqlPool (connectionString dbSetup) 1 $ liftSqlPersistMPool $ keepAlive ref)
  return ref
  where
    keepAlive ref = do
      res <- liftIO $ readIORef ref
      if res == Working
        then do
          time <- liftIO getCurrentTime
          updateFunction time
          transactionSave
          liftIO $ threadDelay (1000000 * keepAliveTimeout)
          keepAlive ref
        else deletionFunction

waitForSlaves :: (ExperimentDef a) => Experiments a -> ReaderT SqlBackend (LoggingT (ExpM a)) Bool
waitForSlaves exps = do
  pid <- liftIO getProcessID
  hostName <- T.pack <$> liftIO getHostName
  let notSelf (ExpExecutionLock _ h p _) = not (h == hostName && p == fromIntegral pid)
  expIds <- selectKeysList [ExpExps ==. expsId] []
  waitForSlaves' notSelf expIds
  where
    expsId = exps ^. experimentsKey
    waitForSlaves' notSelf expIds = do
      locks <- filter notSelf . fmap entityVal <$> selectList [ExpExecutionLockExp <-. expIds] []
      if null locks
        then return True
        else do
          time <- liftIO getCurrentTime
          let workingSlaves = filter (\l -> diffUTCTime time (l ^. expExecutionLockLastAliveSign) <= 2 * keepAliveTimeout) locks
          if null workingSlaves
            then return False -- a slive must have died as it didn't delete the lock
            else do
              $(logInfo) "Waiting for slaves. List of slaves currently working: "
              mapM_ printInfoSlave locks
              liftIO $ threadDelay (1000000 * keepAliveTimeout)
              waitForSlaves' notSelf expIds
    printInfoSlave (ExpExecutionLock _ host pid _) = $(logInfo) $ "Slave from host " <> tshow host <> " with process ID " <> tshow pid

