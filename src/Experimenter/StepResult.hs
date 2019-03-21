{-# LANGUAGE TemplateHaskell #-}

module Experimenter.StepResult where

import           Control.Lens
import qualified Data.Text    as T

data StepResult = StepResult
  { _resultName  :: T.Text
  , _resultValue :: Double
  }
makeLenses ''StepResult


data Measure = Measure
  { _measurePeriod  :: Integer
  , _measureResults :: [StepResult]
  }


