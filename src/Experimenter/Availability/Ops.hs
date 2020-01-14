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
mkAvailableList (AvailableList (nr, xs) src)       = return $ AvailableList (nr, xs) src
mkAvailableList (AvailableListOnDemand (_, query)) = (\xs -> AvailableList (length xs, xs) query) <$> C.runConduit (query GetAll C..| CL.consume)

mkTransientlyAvailableList :: (Monad m) => AvailabilityList m b -> DB m [b]
mkTransientlyAvailableList (AvailableList (_,x) _)             = return x
mkTransientlyAvailableList (AvailableListOnDemand (_,query)) = C.runConduit $ query GetAll C..| CL.consume

lengthAvailabilityList :: AvailabilityList m b -> Int
lengthAvailabilityList (AvailableList (nr, _) _)       = nr
lengthAvailabilityList (AvailableListOnDemand (nr, _)) = nr

srcAvailableList :: (Monad m) => AvailabilityList m a -> ConduitT () a (DB m) ()
srcAvailableList (AvailableList (_, xs) _)       = CL.sourceList xs
srcAvailableList (AvailableListOnDemand (_,src)) = src GetAll

srcAvailableListWhere :: AvailabilityListWhere -> AvailabilityList m a -> ConduitT () a (DB m) ()
srcAvailableListWhere where' (AvailableList _ src)           = src where'
srcAvailableListWhere where' (AvailableListOnDemand (_,src)) = src where'

