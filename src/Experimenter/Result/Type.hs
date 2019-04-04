{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}

module Experimenter.Result.Type where

import           Experimenter.Experiment
import           Experimenter.Input
import           Experimenter.Measure
import           Experimenter.Models
import           Experimenter.Parameter
import           Experimenter.Setup

import           Control.Lens
import qualified Data.Text               as T
import           Data.Time
import           System.Random

data ResultDataKey
  = ResultDataPrep (Key PrepResultData)
  | ResultDataWarmUp (Key WarmUpResultData)
  | ResultDataRep (Key RepResultData)

data ResultData a = ResultData
  { _resultDataKey   :: ResultDataKey
  , _startTime       :: !UTCTime
  , _endTime         :: !(Maybe UTCTime)
  , _startRandGen    :: StdGen
  , _endRandGen      :: Maybe StdGen
  , _inputValues     :: ![Input a]
  , _results         :: ![Measure]
  , _startState      :: !a
  , _endState        :: !(Maybe a)    -- ^ state at end of warm-up phase
  , _startInputState :: !(InputState a)
  , _endInputState   :: !(Maybe (InputState a))
  }
makeLenses ''ResultData

data ReplicationResult a = ReplicationResult
  { _replicationResultKey :: !(Key RepResult)
  , _replicationNumber    :: !Int
  , _warmUpResults        :: Maybe (ResultData a)
  , _evalResults          :: Maybe (ResultData a)
  }
makeLenses ''ReplicationResult

data ExperimentResult a = ExperimentResult
  { _experimentResultKey :: !(Key ExpResult)
  , _repetitionNumber    :: !Int
  , _preparationResults  :: Maybe (ResultData a)
  , _evaluationResults   :: ![ReplicationResult a]
  }
makeLenses ''ExperimentResult


data Experiment a = Experiment
  { _experimentKey       :: !(Key Exp)
  , _experimentNumber    :: !Int
  , _experimentStartTime :: !UTCTime
  , _experimentEndTime   :: !(Maybe UTCTime)
  , _parameterSetup      :: ![ParameterSetting a]
  , _experimentResults   :: ![ExperimentResult a]
  }
makeLenses ''Experiment


data Experiments a = Experiments
  { _experimentsKey               :: !(Key Exps)
  , _experimentsName              :: !T.Text
  , _experimentsStartTime         :: !UTCTime
  , _experimentsEndTime           :: !(Maybe UTCTime)
  , _experimentsSetup             :: !ExpsSetup
  , _experimentsParameters        :: ![ParameterSetup a]
  , _experimentsInitialState      :: !a -- ^ state at period 0
  , _experimentsInitialInputState :: !(InputState a)
  , _experiments                  :: [Experiment a]
  }
makeLenses ''Experiments
