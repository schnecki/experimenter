{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}

module Experimenter.Availability.Ops where

import           Experimenter.Availability.Type

import           Conduit                        as C
import           Data.Conduit.List              as CL

import           Experimenter.DB

mkAvailable :: (Monad m) => Availability m b -> DB m (Availability m b)
mkAvailable (AvailableOnDemand query) = Available <$> query
mkAvailable (Available x)             = return (Available x)

mkTransientlyAvailable :: (Monad m) => Availability m b -> DB m b
mkTransientlyAvailable (Available x)             = return x
mkTransientlyAvailable (AvailableOnDemand query) = query

mkAvailableList :: (Monad m) => AvailabilityList m b -> DB m (AvailabilityList m b)
mkAvailableList (AvailableList (nr, xs))       = return $ AvailableList (nr, xs)
mkAvailableList (AvailableListOnDemand (_, query)) = (\xs -> AvailableList (length xs, xs)) <$> C.runConduit (query C..| CL.consume)

mkTransientlyAvailableList :: (Monad m) => AvailabilityList m b -> DB m [b]
mkTransientlyAvailableList (AvailableList (_,x))             = return x
mkTransientlyAvailableList (AvailableListOnDemand (_,query)) = C.runConduit $ query C..| CL.consume


lengthAvailabilityList :: AvailabilityList m b -> Int
lengthAvailabilityList (AvailableList (nr, _))         = nr
lengthAvailabilityList (AvailableListOnDemand (nr, _)) = nr

