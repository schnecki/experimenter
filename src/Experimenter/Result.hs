{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}


module Experimenter.Result where

import           Experimenter.Models
import           Experimenter.Parameter
import           Experimenter.StepResult

import           Control.Lens
import qualified Data.Text               as T
import           Data.Time
import           System.Random


data ReplicationResult a = ReplicationResult
  { _replicationNumber          :: Integer
  , _replicationRandomGenerator :: forall g . (Show g, Read g, RandomGen g) => g
  , _warmUpResults              :: [Measure]
  , _warmUpEndState             :: a    -- ^ state at end of warm-up phase
  , _warmUpEndTime              :: UTCTime
  , _replicationResults         :: [Measure]
  , _replicationEndState        :: a    -- ^ state at end of experiment
  , _replicationEndTime         :: UTCTime
  }
makeLenses ''ReplicationResult

data ExperimentResult a = ExperimentResult
  { _experimentRepetitionNumber :: Integer
  , _preparationStartTime       :: UTCTime
  , _preparationResults         :: [Measure]
  , _preparationEndTime         :: UTCTime
  , _preparationEndState        :: a -- ^ state after preparation phase
  , _evaluationResults          :: [ReplicationResult a]
  }
makeLenses ''ExperimentResult


data ExperimentResults a = ExperimentResults
  { _experimentStartTime        :: UTCTime
  , _experimentEndTime          :: UTCTime
  , _experimentName             :: T.Text
  , _experimentSetup            :: Setup
  , _experimentResultParameters :: forall b . (ParameterType b) => [Parameter a b]
  , _experimentInitialState     :: a -- ^ state at period 0

  }
makeLenses ''ExperimentResults
