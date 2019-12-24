{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}

module Experimenter.Availability.Ops where

import           Experimenter.Availability.Type

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Database.Persist.Postgresql    (SqlBackend)


lengthAvailabilityList :: AvailabilityList m b -> Int
lengthAvailabilityList (nr, _) = nr

mkAvailableList :: (Monad m, Foldable t) => (Int, Availability m (t b)) -> ReaderT SqlBackend (LoggingT m) (Int, Availability m (t b))
mkAvailableList (nr, Available xs)         = return (nr, Available xs)
mkAvailableList (_, AvailableOnDemand query) = (\xs -> (length xs, Available xs)) <$> query

mkAvailable :: (Monad m) => Availability m b -> ReaderT SqlBackend (LoggingT m) (Availability m b)
mkAvailable (Available xs)            = return (Available xs)
mkAvailable (AvailableOnDemand query) = Available <$> query

mkTransientlyAvailable :: (Monad m) => Availability m b -> ReaderT SqlBackend (LoggingT m) b
mkTransientlyAvailable (Available xs)            = return xs
mkTransientlyAvailable (AvailableOnDemand query) = query
