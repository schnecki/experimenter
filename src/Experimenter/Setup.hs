{-# LANGUAGE TemplateHaskell #-}
module Experimenter.Setup where


import           Control.Lens
import qualified Data.Text           as T

import           Experimenter.Models


data ExperimentSetup = ExperimentSetup
  { _experimentBaseName         :: T.Text         -- ^ Base name of experiment.
  , _experimentRepetitions      :: Int
  , _preparationSteps           :: Maybe Int
  , _evaluationWarmUpSteps      :: Maybe Int
  , _evaluationSteps            :: Int
  , _evaluationReplications     :: Int
  , _maximumParallelEvaluations :: Int
  }
makeLenses ''ExperimentSetup

-- toExpSetup :: ExperimentSetup -> ExpSetup
-- toExpSetup (ExperimentSetup repet prep warm eval repli par) =

