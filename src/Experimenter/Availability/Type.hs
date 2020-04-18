{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}

module Experimenter.Availability.Type where

import           Control.DeepSeq
import           Data.Conduit
import           Database.Esqueleto  as E
import           Database.Persist
import           Experimenter.Models

import           Experimenter.DB

data Availability m b
  = Available !b
  | AvailableOnDemand !(DB m b)

instance (Show b) => Show (Availability m b) where
  show (Available b)         = show b
  show (AvailableOnDemand _) = "Available on demand"

instance (NFData b) => NFData (Availability m b) where
  rnf (Available b)          = rnf b
  rnf (AvailableOnDemand !_) = ()


type AggregateFunction = E.SqlExpr (E.Value Double) -> E.SqlExpr (E.Value (Maybe Double))


data AvailabilityListWhere
  = GetAll
  | PrepInputWhere     !(E.SqlExpr (Entity PrepInput)     -> E.SqlExpr (Entity PrepInputValue)   -> E.SqlQuery ())
  | WarmUpInputWhere   !(E.SqlExpr (Entity WarmUpInput)   -> E.SqlExpr (Entity WarmUpInputValue) -> E.SqlQuery ())
  | RepInputWhere      !(E.SqlExpr (Entity RepInput)      -> E.SqlExpr (Entity RepInputValue)    -> E.SqlQuery ())
  | PrepMeasureWhere   !(E.SqlExpr (Entity PrepMeasure)   -> E.SqlExpr (Entity PrepResultStep)   -> E.SqlQuery ())
  | WarmUpMeasureWhere !(E.SqlExpr (Entity WarmUpMeasure) -> E.SqlExpr (Entity WarmUpResultStep) -> E.SqlQuery ())
  | RepMeasureWhere    !(E.SqlExpr (Entity RepMeasure)    -> E.SqlExpr (Entity RepResultStep)    -> E.SqlQuery ())


instance Show AvailabilityListWhere where
  show GetAll{}             = "GetAll"
  show PrepInputWhere{}     = "PrepInputWhere"
  show WarmUpInputWhere{}   = "WarmUpInputWhere"
  show RepInputWhere{}      = "RepInputWhere"
  show PrepMeasureWhere{}   = "PrepMeasureWhere"
  show WarmUpMeasureWhere{} = "WarmUpMeasureWhere"
  show RepMeasureWhere{}    = "RepMeasureWhere"


data AvailabilityList m b
  = AvailableList !(Int, [b]) (AvailabilityListWhere -> ConduitT () b (DB m) ())
  | AvailableListOnDemand (Int, AvailabilityListWhere -> ConduitT () b (DB m) ())


instance (Show b) => Show (AvailabilityList m b) where
  show (AvailableList b _)       = show b
  show (AvailableListOnDemand _) = "AvailableList on demand"

instance (NFData b) => NFData (AvailabilityList m b) where
  rnf (AvailableList b _)            = rnf b
  rnf (AvailableListOnDemand (nr,_)) = rnf nr

