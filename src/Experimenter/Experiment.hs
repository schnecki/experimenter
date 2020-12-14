{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module Experimenter.Experiment where


import           Control.DeepSeq
import           Control.Monad.IO.Unlift
import           Data.Serialize          (Serialize)
import           Data.Kind
import           System.Random.MWC

import           Experimenter.Parameter
import           Experimenter.StepResult


type Period = Int
type ExperimentNumber = Int
type RepetitionNumber = Int
type ReplicationNumber = Int

data Phase
  = PreparationPhase
  | WarmUpPhase
  | EvaluationPhase
  deriving (Eq, Ord, Show, Enum)


class (Monad (ExpM a), MonadUnliftIO (ExpM a), NFData a, NFData (InputState a), NFData (InputValue a), Serialize (InputValue a), Serialize (InputState a), Serialize (Serializable a)) => ExperimentDef a where

  -- | Monad to run experiments in.
  type ExpM a :: (Type -> Type)

  -- | Type that is used to serialize the current state.
  type Serializable a :: Type

  -- | Function to convert to a serializable object
  serialisable :: a -> ExpM a (Serializable a)

  -- | Function to convert from a serializable object
  deserialisable :: Serializable a -> ExpM a a


  -- | Type of input values to the experiment.
  type InputValue a :: Type        -- ^ The input to the system for running a step.
  type InputState a :: Type        -- ^ Can be used to save a information from one call to `generateInput` to the next.


  -- | Generate some input values and possibly modify state. This function can be used to change the state. It is called
  -- before `runStep` and its output is used to call `runStep`.
  generateInput :: GenIO -> a -> InputState a -> Period -> (ExpM a) (InputValue a, InputState a)

  -- | Run a step of the environment and return new state and result.
  runStep :: Phase -> a -> InputValue a -> Period -> (ExpM a) ([StepResult], a)

  -- | Provides the parameter setting.
  parameters :: a -> [ParameterSetup a]

  -- | This function defines how to find experiments that can be resumed. Note that the experiments name and experiment
  -- info parameters are always comparison factors, that is, experiments with different names or info parameters are
  -- unequal. The default is always True.
  equalExperiments :: (a, InputState a) -> (a, InputState a) -> Bool
  default equalExperiments :: (a, InputState a) -> (a, InputState a) -> Bool
  equalExperiments _ _ = True


  -- HOOKS

  -- | Function to call on the state before the preparation. This function is only executed if the preparation phase
  -- exists (that is >0 preparation steps) and is started from period 0!
  beforePreparationHook :: ExperimentNumber -> RepetitionNumber -> GenIO -> a -> ExpM a a
  default beforePreparationHook :: ExperimentNumber -> RepetitionNumber -> GenIO -> a -> ExpM a a
  beforePreparationHook _ _ _ = return

  -- | Function to call on the state before the warm up phase. This function is only executed if a warm up phase exists
  -- (that is >0 warm-up steps) and is initialised, which happens on the first time it is started!
  beforeWarmUpHook :: ExperimentNumber -> RepetitionNumber -> ReplicationNumber -> GenIO -> a -> ExpM a a
  default beforeWarmUpHook :: ExperimentNumber -> RepetitionNumber -> ReplicationNumber -> GenIO -> a -> ExpM a a
  beforeWarmUpHook _ _ _ _ = return


  -- | Function to call on the state before the evaluation phase. This function is only executed if the evaluation phase
  -- exists (that is >0 evaluation steps) and is initialised which happens on the first time it is started!
  beforeEvaluationHook :: ExperimentNumber -> RepetitionNumber -> ReplicationNumber -> GenIO -> a -> ExpM a a
  default beforeEvaluationHook :: ExperimentNumber -> RepetitionNumber -> ReplicationNumber -> GenIO -> a -> ExpM a a
  beforeEvaluationHook _ _ _ _ = return


  -- | Function to call after the preparation phase, e.g. it can be used to move files. This function is only executed if
  -- the preparation phase is updated. The first parameter is the input state and is only used for type checking.
  afterPreparationHook :: a -> ExperimentNumber -> RepetitionNumber -> IO ()
  default afterPreparationHook :: a -> ExperimentNumber -> RepetitionNumber -> IO ()
  afterPreparationHook _ _ _ = return ()


  -- | Function to call after the warmUp phase, e.g. it can be used to move files. This function is only executed if
  -- the warmUp phase is updated. The first parameter is the input state and is only used for type checking.
  afterWarmUpHook :: a -> ExperimentNumber -> RepetitionNumber -> ReplicationNumber -> IO ()
  default afterWarmUpHook :: a -> ExperimentNumber -> RepetitionNumber -> ReplicationNumber -> IO ()
  afterWarmUpHook _ _ _ _ = return ()


  -- | Function to call after the evaluation phase, e.g. it can be used to move files. This function is only executed if
  -- the evaluation phase is updated. The first parameter is the input state before the evaluation and is only used for type checking.
  afterEvaluationHook :: a -> ExperimentNumber -> RepetitionNumber -> ReplicationNumber -> IO ()
  default afterEvaluationHook :: a -> ExperimentNumber -> RepetitionNumber -> ReplicationNumber -> IO ()
  afterEvaluationHook _ _ _ _ = return ()
