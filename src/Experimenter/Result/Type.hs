{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}

module Experimenter.Result.Type where

import           Experimenter.Experiment
import           Experimenter.Input
import           Experimenter.Measure
import           Experimenter.Models
import           Experimenter.Parameter
import           Experimenter.Setup

import           Control.DeepSeq
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Int
import           Data.Serialize
import qualified Data.Text                   as T
import           Data.Time
import           Database.Persist.Postgresql (SqlBackend)
import           GHC.Generics
import           System.Random

data ResultDataKey
  = ResultDataPrep (Key PrepResultData)
  | ResultDataWarmUp (Key WarmUpResultData)
  | ResultDataRep (Key RepResultData)
  deriving (Eq, Show)

data Availability a b
  = (ExperimentDef a) =>
    Available b
  | AvailableFromDB (ReaderT SqlBackend (LoggingT (ExpM a)) b)

instance NFData b => NFData (Availability a b) where
  rnf (Available b)            = rnf b
  rnf (AvailableFromDB !query) = ()

data ResultData a = ResultData
  { _resultDataKey   :: !ResultDataKey
  , _startTime       :: !UTCTime
  , _endTime         :: !(Maybe UTCTime)
  , _startRandGen    :: !StdGen
  , _endRandGen      :: !(Maybe StdGen)
  , _inputValues     :: !(Int, Availability a [Input a])
  , _results         :: !(Int, Availability a [Measure])
  , _startState      :: !a         -- TODO: Availability for startState & endState
  , _endState        :: !(Maybe a)    -- ^ state at end of warm-up phase
  , _startInputState :: !(InputState a)
  , _endInputState   :: !(Maybe (InputState a))
  }
makeLenses ''ResultData

instance NFData a => NFData (ResultData a) where
  rnf (ResultData !k st end !g !endG !inpVal !res stSt endSt !stInpSt !endInpSt) =
    rnf st `seq` rnf end `seq` rnf res `seq` rnf stSt `seq` rnf endSt

data ReplicationResult a = ReplicationResult
  { _replicationResultKey :: !(Key RepResult)
  , _replicationNumber    :: !Int
  , _warmUpResults        :: !(Maybe (ResultData a))
  , _evalResults          :: !(Maybe (ResultData a))
  }
makeLenses ''ReplicationResult

instance NFData a => NFData (ReplicationResult a) where
  rnf (ReplicationResult !k nr wm ev) = rnf nr `seq` rnf wm `seq` rnf ev

data ExperimentResult a = ExperimentResult
  { _experimentResultKey :: !(Key ExpResult)
  , _repetitionNumber    :: !Int
  , _preparationResults  :: !(Maybe (ResultData a))
  , _evaluationResults   :: ![ReplicationResult a]
  }
makeLenses ''ExperimentResult

instance NFData a => NFData (ExperimentResult a) where
  rnf (ExperimentResult !k nr prep ev) = rnf nr `seq` rnf prep `seq` rnf ev


data Experiment a = Experiment
  { _experimentKey       :: !(Key Exp)
  , _experimentNumber    :: !Int
  , _experimentStartTime :: !UTCTime
  , _experimentEndTime   :: !(Maybe UTCTime)
  , _parameterSetup      :: ![ParameterSetting a]
  , _experimentResults   :: ![ExperimentResult a]
  }
makeLenses ''Experiment

instance NFData a => NFData (Experiment a) where
  rnf (Experiment !k nr stT endT set res) = rnf nr `seq` rnf stT `seq` rnf endT `seq` rnf set `seq` rnf res


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

instance NFData a => NFData (Experiments a) where
  rnf (Experiments !k name stT endT set param !initSt !initInp exps) = rnf name `seq` rnf stT `seq` rnf endT `seq` rnf initSt `seq` rnf exps

instance Serialize StdGen where
  put g = put (show g)
  get = do
    gTxt <- get
    return (read gTxt)


