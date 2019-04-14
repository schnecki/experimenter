{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}

module Experimenter.Eval.Type where

import           Control.Lens             hiding (Over)
import qualified Data.Text                as T


import           Experimenter.Result.Type

-- data OrderedBy = OrderedByStandardOrder
--                | OrderedByFun (Double -> Double -> Ordering)

-- instance Show OrderedBy where
--   show OrderedByStandardOrder = "OrderedByStandardOrder"
--   show (OrderedByFun _)       = "OrderByFun <fun>"

data Over a
  = Replications
  | Periods
  | BestXExperimentEvaluations Int (ExperimentResult a -> ExperimentResult a -> Ordering)

instance Show (Over a) where
  show Replications                      = "Replications"
  show Periods                           = "Periods"
  -- show Evaluations    = "Evaluations"
  show (BestXExperimentEvaluations nr _) = "(BestXExperimentEvaluations " <> show nr <> ")"

data Evaluation a
  = Of T.Text
  | MeanOver (Over a) (Evaluation a)
  | StdDevOver (Over a) (Evaluation a)
  | SumOver (Over a) (Evaluation a)
  | Div (Evaluation a) (Evaluation a)
  deriving (Show)


-- getOver :: Evaluation a -> Maybe (Over a)
-- getOver (MeanOver over _)   = Just over
-- getOver (StdDevOver over _) = Just over
-- getOver (SumOver over _)    = Just over
-- getOver _                   = Nothing


example :: Evaluation a
example = MeanOver Replications (SumOver Periods (Of "NrEarly") `Div` SumOver Periods (Of "NrOrders"))

example2 :: Evaluation a
example2 = MeanOver Replications (Of "X")

-- example3 :: Evaluation          -- List of periods * list of values: [EvalResult]
-- example3 = MeanOver Evaluations (Of "X")


-- data EvalResult
--   = EvalReducedValue { _evalType         :: Evaluation
--                      , _evalVariableName :: T.Text
--                      , _evalValue        :: Double }
-- makeLenses ''EvalResult

-- getEvalValue :: EvalResult -> Double
-- getEvalValue (EvalVector _ _ _ v) = v
-- getEvalValue (EvalScalar _ _ v)   = v

data Unit = UnitPeriods | UnitReplications | UnitExperiments


data EvalResults a
  = EvalVector Unit [EvalResults a] -- ^ For each X
  | EvalValue { _evalType         :: Evaluation a
              , _evalVariableName :: T.Text
              , _evalX            :: Either Int Double -- ^ Either period or xValue.
              , _evalY            :: Double }
  | EvalReducedValue { _evalType         :: Evaluation a
                     , _evalVariableName :: T.Text
                     , _evalValue        :: Double }
makeLenses ''EvalResults

data ExperimentEvals a = ExperimentEval
  { _evalExperimentNumber  :: Int
  , _evalExperimentResults :: [EvalResults a]
  }

data Evals a = Evals
  { _evalsExperiment :: Experiments a
  , _evalsResults    :: [ExperimentEvals a] -- ^ Each result corresponds to one experiment, except if the data is reduced
                                          -- over the experiments.
  }
makeLenses ''Evals


