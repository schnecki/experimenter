{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Experimenter.Measure where

import           Control.DeepSeq
import           Control.Lens
import           GHC.Generics

import           Experimenter.StepResult

data Measure = Measure
  { _measurePeriod  :: !Int
  , _measureResults :: ![StepResult]
  } deriving (Generic)
makeLenses ''Measure

instance NFData Measure where
  rnf (Measure !p !r) = rnf p `seq` rnf1 r
