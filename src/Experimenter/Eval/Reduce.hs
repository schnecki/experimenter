{-# LANGUAGE OverloadedStrings #-}

module Experimenter.Eval.Reduce
    ( reduceUnary
    , reduceBinary
    ) where

import           Control.Lens             hiding (Cons, over)

import           Experimenter.Eval.Type
import           Experimenter.Result.Type

reduceUnary :: Evaluation a -> EvalResults a -> EvalResults a
reduceUnary = undefined
-- reduceUnary (MeanOver over _) res = EvalScalar tp nm val
--   where
--     val = sum (map getEvalValue res) / fromIntegral (length res)
--     tp = MeanOver over (res ^?! _head . evalType)
--     nm = res ^?! _head . evalVariableName
-- reduceUnary (StdDevOver over _) res = EvalScalar tp nm val
--   where
--     vals = map getEvalValue res
--     mean = sum vals / fromIntegral (length res)
--     val = sum (map ((^ (2::Int)) . subtract mean) vals) / max 1 (fromIntegral $ length res-1)
--     tp = StdDevOver over (res ^?! _head . evalType)
--     nm = res ^?! _head . evalVariableName
-- reduceUnary (SumOver over _) res = EvalScalar tp nm val
--   where
--     val = sum (map getEvalValue res)
--     tp = SumOver over (res ^?! _head . evalType)
--     nm = res ^?! _head . evalVariableName
-- reduceUnary eval _ = error $ "unexpected reduce: " ++ show eval


reduceBinary :: Evaluation a -> EvalResults a -> EvalResults a -> EvalResults a
reduceBinary = undefined
-- reduceDinary Div{} (EvalScalar tp1 nm1 val1) (EvalScalar tp2 nm2 val2) = EvalValue $ EvalScalar tp nm val
--   where
--     tp = Div tp1 tp2
--     nm = "(" <> nm1 <> ") / (" <> nm2 <> ")"
--     val = val1 / val2
-- reduceBinary Div{} (EvalVector tp1 nm1 x1 val1) (EvalScalar tp2 nm2 val2) = EvalValue $ EvalVector tp nm x1 val
--     where
--       tp = Div tp1 tp2
--       nm = "(" <> nm1 <> ") / (" <> nm2 <> ")"
--       val = val1 / val2
-- reduceBinary Div {} (EvalVector tp1 nm1 x1 val1) (EvalVector tp2 nm2 x2 val2)
--   | x1 /= x2 = error $ "Dividing vectors with different X values: " <> show x1 <> " and " <> show x2
--   | otherwise = EvalVector tp nm x1 val
--   where
--     tp = Div tp1 tp2
--     nm = "(" <> nm1 <> ") / (" <> nm2 <> ")"
--     val = val1 / val2
