{-# LANGUAGE TemplateHaskell #-}
module Experimenter.ExperimentSetup where


import           Control.Lens


data ExperimentSetup = ExperimentSetup { _preparationSteps       :: Maybe Integer
                                       , _evaluationWarmUpSteps  :: Integer
                                       , _evaluationSteps        :: Integer
                                       , _evaluationReplications :: Integer
                                       }

makeLenses ''ExperimentSetup
