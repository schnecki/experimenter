{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
module Experimenter.Setting where

import           Control.DeepSeq
import           Control.Lens
import           Data.Serialize
import qualified Data.Text       as T
import           GHC.Generics


-- ^ ExperimentInfoParameters can be used to separate experiments from each other. These parameters will be shown in the
-- evaluation output. E.g. testing different feature extraction settings.
data ExperimentInfoParameter =
  forall b. (Show b, Eq b, Serialize b) =>
            ExperimentInfoParameter
              { infoParameterName :: !T.Text
              , infoParameter     :: !b
              }

instance NFData ExperimentInfoParameter where
  rnf (ExperimentInfoParameter t !_) = rnf t


-- ^ Creating an experiment setting with the input state as parameter. This allows dynamically setting values.
type MkExperimentSetting a = a -> ExperimentSetting

data ExperimentSetting = ExperimentSetting
  { _experimentBaseName             :: !T.Text                    -- ^ Base name of experiment.
  , _experimentInfoParameters       :: ![ExperimentInfoParameter] -- ^ List of information parameters defining the experiment. Adding more information does not effect equality. Thus, this is only used
                                                                  -- as information on the report.
  , _experimentRepetitions          :: !Int                       -- ^ Repetitions of each experiment (>=1).
  , _preparationSteps               :: !Int                       -- ^ Preparation phase length (e.g. learning phase) (>=0).
  , _evaluationWarmUpSteps          :: !Int                       -- ^ Warm up phase length before each evaluation of each experiment (>=0).
  , _evaluationSteps                :: !Int                       -- ^ Evaluation phase length (>=0).
  , _evaluationReplications         :: !Int                       -- ^ Replications of evaluation for each experiment (>=1).
  , _evaluationMaxStepsBetweenSaves :: !(Maybe Int) -- ^ Maximum number of steps to take until the data is saved. If nothing then something between 500 and 50000 will be used, depending on the time
                                                    -- required for saving and running. Setting a value may slow down the execution, but limits the memory requirement tremendously.
  } deriving (Generic, NFData)
makeLenses ''ExperimentSetting
