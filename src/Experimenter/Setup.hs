{-# LANGUAGE TemplateHaskell #-}
module Experimenter.Setup where


import           Control.Lens
import qualified Data.Text           as T

import           Experimenter.Models


data ExperimentSetup = ExperimentSetup
  { _experimentBaseName         :: T.Text         -- ^ Base name of experiment.
  , _repetitions                :: Integer
  , _preparationSteps           :: Maybe Integer
  , _evaluationWarmUpSteps      :: Maybe Integer
  , _evaluationSteps            :: Integer
  , _evaluationReplications     :: Integer
  , _maximumParallelEvaluations :: Integer
  }
makeLenses ''ExperimentSetup

-- toExpSetup :: ExperimentSetup -> ExpSetup
-- toExpSetup (ExperimentSetup repet prep warm eval repli par) =

