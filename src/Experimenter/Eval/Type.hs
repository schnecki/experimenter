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
  = OverReplications
  | OverPeriods
  | OverExperiments
  | OverBestXExperimentEvaluations Int (ExperimentResult a -> ExperimentResult a -> Ordering)

instance Eq (Over a) where
  OverReplications == OverReplications = True
  OverPeriods == OverPeriods = True
  OverBestXExperimentEvaluations _ _ == OverBestXExperimentEvaluations _ _ = True
  _ == _ = False

instance Show (Over a) where
  show OverReplications                      = "Replications"
  show OverPeriods                           = "Periods"
  show OverExperiments     = "Experiments"
  show (OverBestXExperimentEvaluations nr _) = "(BestXExperimentEvaluations " <> show nr <> ")"

-- | Definition of statisics. Is used to define the desired output.

data StatsDef a
  = Mean (Over a) (Of a)
  | StdDev (Over a) (Of a)
  | Sum (Over a) (Of a)
  | Id (Of a)
  deriving (Show)

data Of a
  = Of T.Text
  | Stats (StatsDef a)
  | Div (Of a) (Of a)
  | Add (Of a) (Of a)
  | Sub (Of a) (Of a)
  | Mult (Of a) (Of a)
  deriving (Show)

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
  | UnitExperiments
  | UnitBestExperiments Int
  deriving (Show)

data EvalResults a
  = EvalVector { _evalType   :: StatsDef a
               , _evalUnit   :: Unit -- ^ For each X
               , _evalValues :: [EvalResults a]
               }
  | EvalValue { _evalType         :: StatsDef a
              , _evalVariableName :: T.Text
              , _evalX            :: Either Int Double -- ^ Either period or xValue.
              , _evalY            :: Double }
  | EvalReducedValue { _evalType  :: StatsDef a
                     -- , _evalVariableName :: T.Text
                     , _evalValue :: Double }
  deriving (Show)
makeLenses ''EvalResults

getEvalValue :: EvalResults a -> [Double]
getEvalValue (EvalVector _ _ xs)    = concatMap getEvalValue xs
getEvalValue (EvalValue _ _ _ y)    = [y]
getEvalValue (EvalReducedValue _ y) = [y]

getEvalType :: (Over a -> Of a -> StatsDef a) -> EvalResults a -> StatsDef a
getEvalType f (EvalVector tp unit _)     = f (fromUnit unit) (Stats tp)
  where fromUnit UnitPeriods = OverPeriods
        fromUnit UnitReplications = OverReplications
        fromUnit UnitExperiments = OverExperiments
        fromUnit (UnitBestExperiments nr) = OverBestXExperimentEvaluations nr (error "compare function in BestXExperimentEvaluations may not be used")
getEvalType _ (EvalValue t _ _ _)      = t
getEvalType _ (EvalReducedValue t _) = t


data ExperimentEvals a = ExperimentEval
  { _evalExperimentNumber  :: Int
  , _evalExperimentResults :: [EvalResults a]
  }
  deriving (Show)
makeLenses ''ExperimentEvals

data Evals a = Evals
  { _evalsExperiment :: R.Experiments a
  , _evalsResults    :: [ExperimentEvals a] -- ^ Each result corresponds to one experiment, except if the data is reduced
                                          -- over the experiments.
  }
makeLenses ''Evals


