{-# LANGUAGE OverloadedStrings #-}

module Experimenter.Eval.Type where

import qualified Data.Text as T

data OrderedBy = OrderedByStandardOrder
               | OrderedByFun (Double -> Double -> Ordering)

data Over = Replications
          | Periods
          | Evaluation Evaluation OrderedBy -- or use T.Text instead of Evaluation?

data Evaluation
  = Of T.Text
  | MeanOver Over Evaluation
  | StdDevOver Over Evaluation
  | SumOver Over Evaluation
  | Div Evaluation Evaluation


example :: Evaluation
example = MeanOver Replications (SumOver Periods (Of "NrEarly") `Div` SumOver Periods (Of "NrOrders"))

example2 :: Evaluation
example2 = MeanOver Replications (Of "X")

