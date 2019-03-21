{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Experimenter.Experiment where


import           Experimenter.Parameter
import           Experimenter.StepResult

import           Control.Lens
import           Data.Serialize          (Get, Putter)
import qualified Data.Text               as T


class Experiment a where

  -- ^ Type of input values to the experiment.
  type Input a :: *

  -- ^ Generate some input values.
  generateInput :: (Monad m) => ([StepResult], a) -> m (Input a)

  -- ^ Preparation (e.g. Loading from saved state, or training phase in ML applications).
  runPreparationStep :: (Monad m) => Maybe (Either (Get a) (a -> Input a -> m ([StepResult], a)))

  -- ^ Save state using serialization.
  serializeState :: Maybe (Putter a)

  -- ^ Run a step of the environment and return new state and result.
  runEvaluationStep :: (Monad m) => a -> Input a -> m ([StepResult], a)

  -- ^ Provides the parameter setting.
  parameters :: forall b . (ParameterType b) => a -> [Parameter a b]

  -- ^ Base name of experiment.
  experimentBaseName :: a -> T.Text


  -- modifyParameter :: a -> ParameterType a -> ParameterType a
