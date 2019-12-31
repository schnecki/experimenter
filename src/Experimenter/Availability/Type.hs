{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}

module Experimenter.Availability.Type where


import           Control.DeepSeq
import           Data.Conduit

import           Experimenter.DB

data Availability m b
  = Available b
  | AvailableOnDemand (DB m b)

instance (Show b) => Show (Availability m b) where
  show (Available b)         = show b
  show (AvailableOnDemand _) = "Available on demand"

instance (NFData b) => NFData (Availability m b) where
  rnf (Available b)          = rnf b
  rnf (AvailableOnDemand !_) = ()


data AvailabilityList m b
  = AvailableList (Int, [b])
  | AvailableListOnDemand (Int, ConduitT () b (DB m) ())


instance (Show b) => Show (AvailabilityList m b) where
  show (AvailableList b)         = show b
  show (AvailableListOnDemand _) = "AvailableList on demand"

instance (NFData b) => NFData (AvailabilityList m b) where
  rnf (AvailableList b)              = rnf b
  rnf (AvailableListOnDemand (nr,_)) = rnf nr

