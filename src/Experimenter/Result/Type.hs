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


data ReplicationResult a = ReplicationResult
  { _replicationResultKey       :: !(Maybe (Key RepResult))
  , _replicationNumber          :: !Int
  , _replicationRandomGenerator :: forall g . (Show g, Read g, RandomGen g) => g
  , _warmUpInputValues          :: ![Input a]
  , _warmUpResults              :: ![Measure]
  , _warmUpEndState             :: !a    -- ^ state at end of warm-up phase
  , _warmUpEndInputState        :: !(InputState a)
  , _warmUpEndTime              :: !UTCTime
  , _replicationInputValues     :: ![Input a]
  , _replicationResults         :: ![Measure]
  , _replicationEndState        :: !a    -- ^ state at end of experiment
  , _replicationEndInputState   :: !(InputState a)
  , _replicationEndTime         :: !UTCTime
  }
makeLenses ''ReplicationResult

data ExperimentResult a = ExperimentResult
  { _experimentResultKey      :: !(Maybe (Key ExpResult))
  , _repetitionNumber         :: !Int
  , _parameterSetup           :: ![ParameterSetting a]
  , _preparationStartTime     :: !UTCTime
  , _preparationEndTime       :: !UTCTime
  , _preparationInputValues   :: ![Input a]
  , _preparationResults       :: ![Measure]
  , _preparationEndState      :: !a -- ^ state after preparation phase
  , _preparationEndInputState :: !(InputState a)
  , _evaluationResults        :: ![ReplicationResult a]
  }
makeLenses ''ExperimentResult


data Experiment a = Experiment
  { _experimentKey               :: !(Key Exp)
  , _experimentName              :: !T.Text
  , _experimentStartTime         :: !UTCTime
  , _experimentEndTime           :: !(Maybe UTCTime)
  , _experimentSetup             :: !ExpSetup
  , _experimentParameters        :: ![ParameterSetup a]
  , _experimentInitialState      :: !a -- ^ state at period 0
  , _experimentInitialInputState :: !(InputState a)
  , _experimentResults           :: ![ExperimentResult a]
  }
makeLenses ''Experiment
