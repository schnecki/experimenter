module Experimenter.Setup where

import           Experimenter.Models

import           Control.Lens


-- share [mkPersist sqlSettings , mkMigrate "migrateAll"] [persistLowerCase|
-- Setup json
--       experiment Int
--       experimentRepetitions Int
--       preparationSteps Int
--       evaluationWarmUpSteps Int Maybe
--       evaluationSteps Int
--       evaluationReplications Int
--       maximumParallelEvaluations Int
--       deriving Show Read Eq                            |]


-- data ExperimentSetup = ExperimentSetup
--   { _experimentRepetitions      :: Integer
--   , _preparationSteps           :: Maybe Integer
--   , _evaluationWarmUpSteps      :: Integer
--   , _evaluationSteps            :: Integer
--   , _evaluationReplications     :: Integer
--   , _maximumParallelEvaluations :: Integer
--   }

