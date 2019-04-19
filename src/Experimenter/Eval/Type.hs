{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}

module Experimenter.Eval.Type where

import           Control.Lens             hiding (Over)
import qualified Data.Text                as T
import           Prelude                  hiding (sum)


import           Experimenter.Result.Type hiding (Experiments)
import qualified Experimenter.Result.Type as R

-- | Over datatype to reduce data vectors.

data Over a
  = OverPeriods
  | OverReplications
  | OverExperimentRepetitions
  | OverBestXExperimentRepetitions Int (ExperimentResult a -> ExperimentResult a -> Ordering)

instance Eq (Over a) where
  OverReplications == OverReplications = True
  OverPeriods == OverPeriods = True
  OverExperimentRepetitions == OverExperimentRepetitions = True
  OverBestXExperimentRepetitions _ _ == OverBestXExperimentRepetitions _ _ = True
  _ == _ = False

instance Show (Over a) where
  show OverReplications                      = "Replications"
  show OverPeriods                           = "Periods"
  show OverExperimentRepetitions     = "Experiments"
  show (OverBestXExperimentRepetitions nr _) = "(BestXExperimentEvaluations " <> show nr <> ")"

instance Ord (Over a) where
  compare OverReplications OverReplications                                 = EQ
  compare OverReplications _                                                = LT
  compare OverPeriods OverReplications                                      = GT
  compare OverPeriods OverPeriods                                           = EQ
  compare OverPeriods _                                                     = LT
  compare OverExperimentRepetitions OverReplications                        = GT
  compare OverExperimentRepetitions OverPeriods                             = GT
  compare OverExperimentRepetitions OverExperimentRepetitions               = EQ
  compare OverExperimentRepetitions _                                       = LT
  compare OverBestXExperimentRepetitions{} OverBestXExperimentRepetitions{} = EQ
  compare OverBestXExperimentRepetitions{} _                                = GT


-- | Definition of statisics. Is used to define the desired output.


data StatsDef a
  = Mean (Over a) (Of a)
  | StdDev (Over a) (Of a)
  | Sum (Over a) (Of a)
  | Id (Of a)
  deriving (Show, Eq, Ord)

data Of a
  = Of T.Text
  | Stats (StatsDef a)
  | Div (Of a) (Of a)
  | Add (Of a) (Of a)
  | Sub (Of a) (Of a)
  | Mult (Of a) (Of a)
  deriving (Show, Eq, Ord)

-- Helper functions for demoting StatsDefs to Ofs.

sum :: Over a -> Of a -> Of a
sum over of' = Stats (Sum over of')

stdDev :: Over a -> Of a -> Of a
stdDev over of' = Stats (StdDev over of')

mean :: Over a -> Of a -> Of a
mean over of' = Stats (Mean over of')


-- | Simple examples on how to use the types

example :: StatsDef a
example = Mean OverReplications (sum OverPeriods (Of "NrEarly") `Div` sum OverPeriods (Of "NrOrders"))

example2 :: StatsDef a
example2 = Mean OverReplications (Of "NrEarly" `Div` Of "NrOrders")


example3 :: StatsDef a
example3 = Mean OverReplications (Of "X")


-- | Datatypes for the evaluation result.

data Unit
  = UnitPeriods
  | UnitReplications
  | UnitExperimentRepetition
  | UnitBestExperimentRepetitions Int
  deriving (Eq, Ord, Show)

data EvalResults a
  = EvalVector { _evalType   :: StatsDef a
               , _evalUnit   :: Unit -- ^ Over which the vector runs.
               , _evalValues :: [EvalResults a]
               }
  | EvalValue { _evalType         :: StatsDef a
              , _evalUnit         :: Unit -- ^ Is always periods.
              , _evalVariableName :: T.Text
              , _evalX            :: Either Int Double -- ^ Either period or xValue.
              , _evalY            :: Double }
  | EvalReducedValue { _evalType  :: StatsDef a
                     , _evalUnit  :: Unit -- ^ Over which was reduced.
                     , _evalValue :: Double }
  deriving (Show)
makeLenses ''EvalResults

data ExperimentEval a = ExperimentEval
  { _evalExperimentNumber  :: Int
  -- , _evalExperimentRepetition :: Int
  , _evalExperimentResults :: [EvalResults a]
  , _evalExperiment        :: Experiment a
  }
makeLenses ''ExperimentEval

instance Show (ExperimentEval a) where
  show x = show (x ^. evalExperimentResults)

data Evals a = Evals
  { _evalsExperiments :: R.Experiments a
  , _evalsResults     :: [ExperimentEval a]
  }
makeLenses ''Evals


-- Helper Functions

getEvalValue :: EvalResults a -> [Double]
getEvalValue (EvalVector _ _ xs)      = concatMap getEvalValue xs
getEvalValue (EvalValue _ _ _ _ y)    = [y]
getEvalValue (EvalReducedValue _ _ y) = [y]

getEvalType :: (Over a -> Of a -> StatsDef a) -> EvalResults a -> StatsDef a
getEvalType f (EvalVector tp unit _)     = f (fromUnit unit) (Stats tp)
  where fromUnit UnitPeriods = OverPeriods
        fromUnit UnitReplications = OverReplications
        fromUnit UnitExperimentRepetition = OverExperimentRepetitions
        fromUnit (UnitBestExperimentRepetitions nr) = OverBestXExperimentRepetitions nr (error "compare function in BestXExperimentEvaluations may not be used")
getEvalType _ (EvalValue t _ _ _ _)      = t
getEvalType _ (EvalReducedValue t _ _) = t

fromOver :: Over a -> Unit
fromOver OverPeriods                           = UnitPeriods
fromOver OverReplications                      = UnitReplications
fromOver OverExperimentRepetitions             = UnitExperimentRepetition
fromOver (OverBestXExperimentRepetitions nr _) = UnitBestExperimentRepetitions nr

-- | Demotes the unit by 1 degree. Thus this calculates the unit of a vector over which it was reduced.
demoteUnit :: Unit -> Maybe Unit
demoteUnit UnitPeriods                     = Nothing
demoteUnit UnitReplications                = Just UnitPeriods
demoteUnit UnitExperimentRepetition        = Just UnitReplications
demoteUnit UnitBestExperimentRepetitions{} = Just UnitReplications
