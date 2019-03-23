{-# LANGUAGE TemplateHaskell #-}

module Experimenter.Measure where

import           Control.Lens

import           Experimenter.StepResult

data Measure = Measure
  { _measurePeriod  :: Int
  , _measureResults :: [StepResult]
  }
makeLenses ''Measure

