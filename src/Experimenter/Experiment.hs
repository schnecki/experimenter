{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module Experimenter.Experiment where


import           Control.DeepSeq
import           Control.Monad.IO.Unlift
import           Data.Serialize          (Serialize)
import           System.Random.MWC

import           Experimenter.Parameter
import           Experimenter.StepResult


type Period = Int
type RepetitionNumber = Int
type ReplicationNumber = Int


class (MonadUnliftIO (ExpM a), NFData a, NFData (InputState a), Serialize (InputValue a), Serialize (InputState a), Serialize (Serializable a)) => ExperimentDef a where

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
  generateInput :: GenIO -> a -> InputState a -> Period -> (ExpM a) (InputValue a, InputState a)

  -- ^ Run a step of the environment and return new state and result.
  runStep :: a -> InputValue a -> Period -> (ExpM a) ([StepResult], a)

  -- ^ Provides the parameter setting.
  parameters :: a -> [ParameterSetup a]

  -- ^ This function defines how to find experiments that can be resumed. Note that the experiments name is always a
  -- comparison factor, that is, experiments with different names are unequal.
  equalExperiments :: (a, InputState a) -> (a, InputState a) -> Bool
  default equalExperiments :: (Eq a, Eq (InputState a)) => (a, InputState a) -> (a, InputState a) -> Bool
  equalExperiments x y = x == y


  -- HOOKS

  -- ^ Function to call on the state before the preparation. This function is only executed if the preparation phase
  -- exists (that is >0 preparation steps) and is started from period 0!
  beforePreparationHook :: RepetitionNumber -> GenIO -> a -> ExpM a a
  default beforePreparationHook :: RepetitionNumber -> GenIO -> a -> ExpM a a
  beforePreparationHook _ _ = return

  -- ^ Function to call on the state before the warm up phase. This function is only executed if a warm up phase exists
  -- (that is >0 warm-up steps) and is initialised, which happens on the first time it is started!
  beforeWarmUpHook :: RepetitionNumber -> ReplicationNumber -> GenIO -> a -> ExpM a a
  default beforeWarmUpHook :: RepetitionNumber -> ReplicationNumber -> GenIO -> a -> ExpM a a
  beforeWarmUpHook _ _ _ = return


  -- ^ Function to call on the state before the evaluation phase. This function is only executed if the evaluation phase
  -- exists (that is >0 evaluation steps) and is initialised which happens on the first time it is started!
  beforeEvaluationHook :: RepetitionNumber -> ReplicationNumber -> GenIO -> a -> ExpM a a
  default beforeEvaluationHook :: RepetitionNumber -> ReplicationNumber -> GenIO -> a -> ExpM a a
  beforeEvaluationHook _ _ _ = return
