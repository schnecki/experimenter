{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}

module Experimenter.Experiment where


import           Experimenter.Parameter
import           Experimenter.StepResult

import           Data.Serialize          (Get, Putter, Serialize)
import qualified Data.Text               as T


class (Serialize (InputValue a)) => ExperimentDef a where

  -- ^ Type of input values to the experiment.
  type InputValue a :: *

  -- ^ Generate some input values.
  generateInput :: (Monad m) => ([StepResult], a) -> m (InputValue a)

  -- ^ Preparation (e.g. Loading from saved state, or training phase in ML applications).
  runPreparationStep :: (Monad m) => Maybe (Either (Get a) (a -> InputValue a -> m ([StepResult], a)))

  -- ^ Save state using serialization.
  serializeState :: Maybe (Putter a)

  -- ^ Run a step of the environment and return new state and result.
  runEvaluationStep :: (Monad m) => a -> InputValue a -> m ([StepResult], a)

  -- ^ Provides the parameter setting.
  parameters :: forall b . (ParameterType b) => a -> [ParameterSetup a b]

  -- ^ Base name of experiment.
  experimentBaseName :: a -> T.Text


  -- modifyParameter :: a -> ParameterType a -> ParameterType a


