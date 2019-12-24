{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}

module Experimenter.Availability.Type where

import           Experimenter.Experiment

import           Control.DeepSeq
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Database.Persist.Postgresql (SqlBackend)


data Availability m b
  = Available b
  | AvailableOnDemand (ReaderT SqlBackend (LoggingT m) b)

-- TODO/Note: m has to be (ExpM a) right now as the Monad stack needs to be flipped

instance (Show b) => Show (Availability m b) where
  show (Available b)         = show b
  show (AvailableOnDemand _) = "Available on demand"

instance NFData b => NFData (Availability m b) where
  rnf (Available b)          = rnf b
  rnf (AvailableOnDemand !_) = ()

type AvailabilityList m b = (Int, Availability m [b]) -- ^ Demand availability of a list with the length fetched.

-- instance Show b => Show (AvailabilityList a b) where
--   show (nr, av) = "List of length " <> show nr <> ": " <> show av

