{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}

module Experimenter.Experiment where


import           Experimenter.Parameter
import           Experimenter.StepResult

import           Data.Serialize          (Get, Putter, Serialize)
import           System.Random


type Period = Int


class (Serialize (InputValue a), Serialize (InputState a), Serialize a) => ExperimentDef a where

  -- ^ Type of input values to the experiment.
  type InputValue a :: *        -- ^ The input to the system for running a step.
  type InputState a :: *        -- ^ Can be used to save a information from one call to `generateInput` to the next.


  -- ^ Generate some input values and possibly modify state. This function can be used to change the state. It is called
  -- before `runStep` and its output is used to call `runStep`.
  generateInput :: (Monad m, RandomGen g) => g -> a -> InputState a -> Period -> m (InputValue a, InputState a)

  -- ^ Save state using serialization.
  serializeState :: Maybe (Putter a)

  -- ^ Run a step of the environment and return new state and result.
  runStep :: (Monad m) => a -> InputValue a -> Period -> m ([StepResult], a)

  -- ^ Provides the parameter setting.
  parameters :: a -> [ParameterSetup a]


  -- ^ Preparation (e.g. Loading from saved state, or training phase in ML applications).
  -- runPreparationStep :: (Monad m) => Maybe (Either (Get a) (a -> InputValue a -> m ([StepResult], a)))
  -- ^ Maybe TODO: Load preparation phase?


