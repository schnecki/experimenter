{-# LANGUAGE TemplateHaskell #-}

module Experimenter.StepResult where

import           Control.Lens
import qualified Data.Text    as T

data StepResult = StepResult
  { _resultName   :: T.Text
  , _resultXValue :: Maybe Double -- ^ If not specified number of steps
  , _resultYValue :: Double
  }
makeLenses ''StepResult


