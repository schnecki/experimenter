{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}

module Experimenter.Experiment where


import           Experimenter.Parameter
import           Experimenter.StepResult

import           Data.Serialize          (Get, Putter, Serialize)
import           System.Random

class (Serialize (InputValue a), Serialize (InputState a), Serialize a) => ExperimentDef a where

  -- ^ Type of input values to the experiment.
  type InputValue a :: *
  type InputState a :: *

  -- ^ Generate some input values.
  generateInput :: (Monad m, RandomGen g) => g -> InputState a -> a -> m (InputValue a, InputState a)

  -- ^ Preparation (e.g. Loading from saved state, or training phase in ML applications).
  runPreparationStep :: (Monad m) => Maybe (Either (Get a) (a -> InputValue a -> m ([StepResult], a)))

  -- ^ Save state using serialization.
  serializeState :: Maybe (Putter a)

  -- ^ Run a step of the environment and return new state and result.
  runEvaluationStep :: (Monad m) => a -> InputValue a -> m ([StepResult], a)

  -- ^ Provides the parameter setting.
  parameters :: a -> [ParameterSetup a]


