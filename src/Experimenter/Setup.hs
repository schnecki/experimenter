module Experimenter.Setup where

import           Experimenter.Models

import           Control.Lens

data ExperimentSetup = ExperimentSetup
  { _repetitions                :: Integer
  , _preparationSteps           :: Maybe Integer
  , _evaluationWarmUpSteps      :: Maybe Integer
  , _evaluationSteps            :: Integer
  , _evaluationReplications     :: Integer
  , _maximumParallelEvaluations :: Integer
  }


-- toExpSetup :: ExperimentSetup -> ExpSetup
-- toExpSetup (ExperimentSetup repet prep warm eval repli par) =

