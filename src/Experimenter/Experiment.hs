{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module Experimenter.Experiment where


import           Control.DeepSeq
import           Control.Monad.IO.Unlift
import           Data.Serialize          (Serialize)
import           System.Random

import           Experimenter.Parameter
import           Experimenter.StepResult


type Period = Int


class (MonadUnliftIO (ExpM a), NFData a, NFData (InputValue a), NFData (InputState a), Serialize (InputValue a), Serialize (InputState a), Serialize (Serializable a)) => ExperimentDef a where

  type ExpM a :: (* -> *)

  type Serializable a :: *      -- ^ Type that is used to serialize the current state.

  -- ^ Function to convert to a serializable object
  serialisable :: a -> ExpM a (Serializable a)

  -- ^ Function to convert from a serializable object
  deserialisable :: Serializable a -> ExpM a a


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

  -- ^ Function to call on the state after the preparation, that is before the warm-up, or in case of no warm-up phase,
  -- the evaluation phase is started. This function can be useful to set different parameters for the evaluation phase
  -- as compared to the preparation (e.g. learning) phase. The default implementation is `id`.
  afterPreparationPhase :: a -> a
  default afterPreparationPhase :: a -> a
  afterPreparationPhase = id


  -- ^ This function defines how to find experiments that can be resumed. Note that the experiments name is always a
  -- comparison factor, that is, experiments with different names are unequal.
  equalExperiments :: (a, InputState a) -> (a, InputState a) -> Bool
  default equalExperiments :: (Eq a, Eq (InputState a)) => (a, InputState a) -> (a, InputState a) -> Bool
  equalExperiments x y = x == y

