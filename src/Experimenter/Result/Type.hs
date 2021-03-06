{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}

module Experimenter.Result.Type where

import           Experimenter.Availability
import           Experimenter.Experiment
import           Experimenter.Input
import           Experimenter.Measure
import           Experimenter.Models
import           Experimenter.Parameter
import           Experimenter.Setting

import           Control.DeepSeq
import           Control.Lens
import qualified Data.Text                 as T
import           Data.Time
import           System.Random.MWC


phaseFromResultDataKey :: ResultDataKey -> Phase
phaseFromResultDataKey ResultDataPrep{}   = PreparationPhase
phaseFromResultDataKey ResultDataWarmUp{} = WarmUpPhase
phaseFromResultDataKey ResultDataRep{}    = EvaluationPhase

data ResultDataKey
  = ResultDataPrep !(Key PrepResultData)
  | ResultDataWarmUp !(Key WarmUpResultData)
  | ResultDataRep !(Key RepResultData)
  deriving (Eq, Ord, Show)

instance NFData ResultDataKey where
  rnf (ResultDataPrep !_)   = ()
  rnf (ResultDataWarmUp !_) = ()
  rnf (ResultDataRep !_)    = ()


data ResultData a = ResultData
  { _resultDataKey   :: !ResultDataKey
  , _startTime       :: !UTCTime
  , _endTime         :: !(Maybe UTCTime)
  , _startRandGen    :: !GenIO
  , _endRandGen      :: !(Maybe GenIO)
  , _inputValues     :: !(AvailabilityList (ExpM a) (Input a))
  , _results         :: !(AvailabilityList (ExpM a) Measure)
  , _startState      :: !(Availability (ExpM a) a)
  , _endState        :: !(Availability (ExpM a) (Maybe a))    -- ^ state at end of run
  , _startInputState :: !(InputState a)
  , _endInputState   :: !(Maybe (InputState a))
  }
makeLenses ''ResultData

instance (ExperimentDef a) => NFData (ResultData a) where
  rnf (ResultData !k st end !_ !_ !inpVal !res _ endSt !_ !_) = rnf k `seq` rnf st `seq` rnf end `seq` rnf inpVal `seq` rnf res `seq` rnf endSt -- `seq` rnf stSt

data ReplicationResult a = ReplicationResult
  { _replicationResultKey :: !(Key RepResult)
  , _replicationNumber    :: !Int
  , _warmUpResults        :: !(Maybe (ResultData a))
  , _evalResults          :: !(Maybe (ResultData a))
  }
makeLenses ''ReplicationResult

instance (ExperimentDef a) => NFData (ReplicationResult a) where
  rnf (ReplicationResult !_ nr wm ev) = rnf nr `seq` rnf1 wm `seq` rnf1 ev

data ExperimentResult a = ExperimentResult
  { _experimentResultKey :: !(Key ExpResult)
  , _repetitionNumber    :: !Int
  , _preparationResults  :: !(Maybe (ResultData a))
  , _evaluationResults   :: ![ReplicationResult a]
  }
makeLenses ''ExperimentResult

instance (ExperimentDef a) => NFData (ExperimentResult a) where
  rnf (ExperimentResult !_ nr prep ev) = rnf nr `seq` rnf1 prep `seq` rnf1 ev

data Experiment a = Experiment
  { _experimentKey       :: !(Key Exp)
  , _experimentNumber    :: !Int
  , _experimentStartTime :: !UTCTime
  , _experimentEndTime   :: !(Maybe UTCTime)
  , _parameterSetup      :: ![ParameterSetting a]
  , _experimentResults   :: ![ExperimentResult a]
  }
makeLenses ''Experiment

instance (ExperimentDef a) => NFData (Experiment a) where
  rnf (Experiment !_ nr stT endT setup res) = rnf nr `seq` rnf stT `seq` rnf1 endT `seq` rnf1 setup `seq` rnf1 res


data Experiments a = Experiments
  { _experimentsKey               :: !(Key Exps)
  , _experimentsName              :: !T.Text
  , _experimentsStartTime         :: !UTCTime
  , _experimentsEndTime           :: !(Maybe UTCTime)
  , _experimentsSetup             :: !ExpsSetup
  , _experimentsParameters        :: ![ParameterSetup a]
  , _experimentsInfoParameters    :: ![ExperimentInfoParameter]
  , _experimentsInitialState      :: !a -- ^ state at period 0
  , _experimentsInitialInputState :: !(InputState a)
  , _experiments                  :: ![Experiment a]
  }
makeLenses ''Experiments

instance (ExperimentDef a) => NFData (Experiments a) where
  rnf (Experiments !_ name ~stT !endT !_ !param !infoParams !initSt !initInp exps) =
    rnf name `seq` rnf stT `seq` rnf endT `seq` map rwhnf param `seq` rnf1 infoParams `seq` rnf initSt `seq` rnf initInp `seq` rnf1 exps

-- instance Serialize GenIO where
--   put g = put (show g)
--   get = do
--     gTxt <- get
--     return (read gTxt)
