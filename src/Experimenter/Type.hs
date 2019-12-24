module Experimenter.Type where

import           Control.Monad.Identity
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Database.Persist.Postgresql (SqlBackend)

import           Experimenter.Experiment


type DB a = ReaderT SqlBackend (LoggingT (ExpM a))

-- type DB m = ReaderT SqlBackend (LoggingT m)
type SimpleDB = ReaderT SqlBackend (LoggingT IO)

-- Need to turn around Monad stack:
-- type ExpDB a = (ExpM a) (ReaderT SqlBackend (LoggingT Identity))
