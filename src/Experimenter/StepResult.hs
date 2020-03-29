{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Experimenter.StepResult where

import           Control.DeepSeq
import           Control.Lens
import qualified Data.Text       as T
import           GHC.Generics

data StepResult = StepResult
  { _resultName   :: !T.Text
  , _resultXValue :: !(Maybe Double) -- ^ If not specified number of steps
  , _resultYValue :: !Double
  } deriving (Generic, NFData)
makeLenses ''StepResult


