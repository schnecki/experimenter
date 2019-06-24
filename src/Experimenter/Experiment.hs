{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Experimenter.Experiment where


import           Experimenter.Parameter
import           Experimenter.StepResult

import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger
import           Data.Serialize          (Serialize)
import           System.Random

type Period = Int


class (MonadUnliftIO (ExpM a), Serialize (InputValue a), Serialize (InputState a), Serialize (Serializable a)) => ExperimentDef a where

  type ExpM a :: (* -> *)

  type Serializable a :: *      -- ^ Type that is used to serialize the current state.

  -- ^ Function to convert to a serializable object
  serialisable :: a -> Serializable a

  -- ^ Function to convert from a serializable object
  deserialisable :: Serializable a -> a


  -- ^ Type of input values to the experiment.
  type InputValue a :: *        -- ^ The input to the system for running a step.
  type InputState a :: *        -- ^ Can be used to save a information from one call to `generateInput` to the next.


  -- ^ Generate some input values and possibly modify state. This function can be used to change the state. It is called
  -- before `runStep` and its output is used to call `runStep`.
  generateInput :: StdGen -> a -> InputState a -> Period -> (ExpM a) (InputValue a, InputState a)

  -- ^ Run a step of the environment and return new state and result.
  runStep :: a -> InputValue a -> Period -> (ExpM a) ([StepResult], a)

  -- ^ Provides the parameter setting.
  parameters :: a -> [ParameterSetup a]

  -- ^ This function defines how to find experiments that can be resumed. Note that the experiments name is always a
  -- comparison factor, that is, experiments with different names are unequal.
  equalExperiments :: (a, InputState a) -> (a, InputState a) -> Bool
  default equalExperiments :: (Eq a, Eq (InputState a)) => (a, InputState a) -> (a, InputState a) -> Bool
  equalExperiments x y = x == y

  -- -- ^ Calculate the objective
  -- objective :: Maybe (Objective a)
  -- default objective :: Maybe (Objective a)
  -- objective = Nothing


-- data Objective a
--   = MaxMean (Over a) (Of a)
--   | MaxStdDev (Over a) (Of a)
--   | MaxSum (Over a) (Of a)
--   | MinMean (Over a) (Of a)
--   | MinStdDev (Over a) (Of a)
--   | MinSum (Over a) (Of a)
--   deriving (Show)


-- fromObjective :: Objective a -> StatsDef a
-- fromObjective (MaxMean   ov o) = Mean ov o
-- fromObjective (MaxStdDev ov o) = StdDev ov o
-- fromObjective (MaxSum    ov o) = Sum ov o
-- fromObjective (MinMean   ov o) = Mean ov o
-- fromObjective (MinStdDev ov o) = StdDev ov o
-- fromObjective (MinSum    ov o) = Sum ov o

