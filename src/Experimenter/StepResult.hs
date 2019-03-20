{-# LANGUAGE TemplateHaskell #-}

module Experimenter.StepResult where

import           Control.Lens
import qualified Data.Text    as T

data ResultObjective = Maximize | Minimize

data StepResult = StepResult
  { _resultObjective :: ResultObjective
  , _resultName      :: T.Text
  , _resultValue     :: Double
  }
makeLenses ''StepResult

