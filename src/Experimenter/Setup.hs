{-# LANGUAGE TemplateHaskell #-}
module Experimenter.Setup where


import           Control.Lens
import qualified Data.Text           as T

import           Experimenter.Models


data ExperimentSetup = ExperimentSetup
  { _experimentBaseName         :: T.Text         -- ^ Base name of experiment.
  , _experimentRepetitions      :: Int            -- ^ Repetitions of each experiment (>=1).
  , _preparationSteps           :: Int            -- ^ Preparation phase length (e.g. learning phase) (>=0).
  , _evaluationWarmUpSteps      :: Int            -- ^ Warm up phase length before each evaluation of each experiment (>=0).
  , _evaluationSteps            :: Int            -- ^ Evaluation phase length (>=0).
  , _evaluationReplications     :: Int            -- ^ Replications of evaluation for each experiment (>=1).
  , _maximumParallelEvaluations :: Int            -- ^ Maximum number of parallel evaluations (>=1).
  }
makeLenses ''ExperimentSetup

