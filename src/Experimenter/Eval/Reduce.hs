{-# LANGUAGE OverloadedStrings #-}

module Experimenter.Eval.Reduce
    ( reduceUnary
    , reduceBinary
    ) where

import           Control.Lens             hiding (Cons, Over, over)

import           Experimenter.Eval.Type   hiding (sum)
import           Experimenter.Result.Type hiding (Experiments)

reduceUnary :: StatsDef a -> EvalResults a -> EvalResults a
reduceUnary (Mean over _) eval = EvalReducedValue (flatten tp) val
  where
    res = getEvalValue eval
    val = sum res / fromIntegral (length res)
    tp = getEvalType Mean eval
reduceUnary (StdDev over _) eval = EvalReducedValue (flatten tp) val
  where
    res = getEvalValue eval
    mean = sum res / fromIntegral (length res)
    val = sqrt $ sum (map ((^ (2::Int)) . subtract mean) res) / max 1 (fromIntegral $ length res-1)
    tp = getEvalType StdDev eval
reduceUnary (Sum over _) eval = EvalReducedValue (flatten $ getEvalType Sum eval) (sum (getEvalValue eval))
reduceUnary eval _ = error $ "unexpected reduce: " ++ show eval

flatten :: StatsDef a -> StatsDef a
flatten (Mean over o)   = flattenStats $ Mean over (flattenOf o)
flatten (StdDev over o) = flattenStats $ StdDev over (flattenOf o)
flatten (Sum over o)    = flattenStats $ Sum over (flattenOf o)
flatten (Id (Stats e))  = flatten e
flatten (Id o)          = flattenStats $ Id $ flattenOf o


flattenOf :: Of a -> Of a
flattenOf (Stats (Id e)) = e
flattenOf e              = e

flattenStats :: StatsDef a -> StatsDef a
flattenStats (Id (Stats e)) = e
flattenStats e              = e


reduceBinary :: Of a -> EvalResults a -> EvalResults a -> EvalResults a
reduceBinary Add{} (EvalValue tp1 _ _ val1) (EvalValue tp2 _ _ val2) = EvalReducedValue (Id $ Stats tp1 `Add` Stats tp2) (val1 + val2)
reduceBinary Sub{} (EvalValue tp1 _ _ val1) (EvalValue tp2 _ _ val2) = EvalReducedValue (Id $ Stats tp1 `Sub` Stats tp2) (val1 - val2)
reduceBinary Mult{} (EvalValue tp1 _ _ val1) (EvalValue tp2 _ _ val2) = EvalReducedValue (Id $ Stats tp1 `Mult` Stats tp2) (val1 * val2)
reduceBinary Div{} (EvalValue tp1 _ _ val1) (EvalValue tp2 _ _ val2) = EvalReducedValue (Id $ Stats tp1 `Div` Stats tp2) (val1 / val2)
reduceBinary eval _ _ = error $ "unexpected binary reduce: " ++ show eval
