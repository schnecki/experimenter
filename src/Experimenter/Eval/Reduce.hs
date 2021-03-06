{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

module Experimenter.Eval.Reduce
    ( reduceUnary
    , reduceUnaryOf
    , reduceBinaryOf
    , transpose
    , flatten
    ) where

import           Control.DeepSeq
import           Control.Lens           hiding (Cons, Over, over)
import qualified Data.List              as L (transpose)

import           Experimenter.Eval.Type hiding (sum)

transpose :: Unit -> [EvalResults a] -> [EvalResults a]
transpose resUnit vs@(EvalValue tp _ _ _ _:_) = map (EvalVector tp resUnit) $ L.transpose $ map (: []) vs
transpose resUnit vs@(EvalReducedValue tp _ _:_) = map (EvalVector tp resUnit) $ L.transpose $ map (: []) vs
transpose resUnit vs@(EvalVector tp _ _:_) = map (EvalVector tp resUnit) $ L.transpose $ map (view evalValues) vs
transpose _ [] = []

reduceUnary :: StatsDef a -> EvalResults a -> EvalResults a
reduceUnary (Mean over _) !eval = force $! EvalReducedValue (flatten tp) (fromOver over) val
  where
    res = getEvalValue eval
    val = sum res / fromIntegral (length res)
    tp = getEvalType Mean eval
reduceUnary (StdDev over _) !eval = force $! EvalReducedValue (flatten tp) (fromOver over) val
  where
    res = getEvalValue eval
    mean' = sum res / fromIntegral (length res)
    val = sqrt $ sum (map ((^ (2::Int)) . subtract mean') res) / max 1 (fromIntegral $ length res-1)
    tp = getEvalType StdDev eval
reduceUnary (Sum over _) !eval = force $! EvalReducedValue (flatten $ getEvalType Sum eval) (fromOver over) (sum (getEvalValue eval))
reduceUnary eval _ = error $ "unexpected reduce: " ++ show eval

flatten :: StatsDef a -> StatsDef a
flatten (Mean over o)   = force $! flattenStats $ Mean over (flattenOf o)
flatten (StdDev over o) = force $! flattenStats $ StdDev over (flattenOf o)
flatten (Sum over o)    = force $! flattenStats $ Sum over (flattenOf o)
flatten (Id (Stats e))  = force $! flatten e
flatten (Id o)          = force $! flattenStats $ Id $ flattenOf o
flatten (Named _ x)     = error $ "Unexpected Named in flatten in Reduce.hs: " ++ show x
flatten (Name _ x)      = error $ "Unexpected Name in flatten in Reduce.hs: " ++ show x


flattenOf :: Of a -> Of a
flattenOf (Stats (Id e)) = e
flattenOf e              = e

flattenStats :: StatsDef a -> StatsDef a
flattenStats (Id (Stats e)) = e
flattenStats e              = e


reduceUnaryOf :: Of a -> EvalResults a -> EvalResults a
reduceUnaryOf First {} (EvalVector tp unit vals) =
  case vals of
    (EvalVector {}:_)          -> force $! EvalVector (Id $ First $ Stats tp) unit [head vals]
    (EvalValue _ _ _ _ v:_)    -> force $! EvalReducedValue (Id $ First $ Stats tp) UnitScalar v
    (EvalReducedValue _ _ v:_) -> force $! EvalReducedValue (Id $ First $ Stats tp) UnitScalar v
    []                         -> error "empty elements in reduceUnaryOf First{}"
reduceUnaryOf Last {} (EvalVector tp unit vals) =
  case vals of
 (EvalVector {}:_)          -> force $! EvalVector (Id $ Last $ Stats tp) unit [last vals]
 (EvalValue _ _ _ _ _:_)    -> force $! EvalReducedValue (Id $ Last $ Stats tp) UnitScalar ((^?! evalY) $ last vals)
 (EvalReducedValue _ _ _:_) -> force $! EvalReducedValue (Id $ Last $ Stats tp) UnitScalar ((^?! evalValue) $ last vals)
 []                         -> error "empty elements in reduceUnaryOf Last{}"
reduceUnaryOf (EveryXthElem nr _) (EvalVector tp unit vals) =
  case vals of
    (EvalVector {}:_)          -> force $! EvalVector (Id $ EveryXthElem nr $ Stats tp) unit (extractEvery nr vals)
    (EvalValue _ _ _ _ _:_)    -> force $! EvalVector (Id $ EveryXthElem nr $ Stats tp) unit (extractEvery nr vals)
    (EvalReducedValue _ _ _:_) -> force $! EvalVector (Id $ EveryXthElem nr $ Stats tp) unit (extractEvery nr vals)
    [] -> error "empty elements in reduceUnaryOf EveryXthElem{}"
  where
    extractEvery m = map snd . filter (\(x, _) -> mod x m == 0) . zip [1 ..]
reduceUnaryOf Length {} (EvalVector tp _ vals) = force $! EvalReducedValue (Id $ Length $ Stats tp) UnitScalar (fromIntegral $ length vals)
reduceUnaryOf eval dt = error $ "unexpected unary reduce: " ++ show eval ++ " on " ++ show dt

reduceBinaryOf :: Of a -> EvalResults a -> EvalResults a -> EvalResults a
reduceBinaryOf Add{}  (EvalValue tp1 u1 _ _ val1) (EvalValue tp2 u2 _ _ val2)       = force $! checkUnitTypes "Add" u1 u2 $ EvalReducedValue (Id $ Stats tp1 `Add` Stats tp2) u1 (val1 + val2)
reduceBinaryOf Sub{}  (EvalValue tp1 u1 _ _ val1) (EvalValue tp2 u2 _ _ val2)       = force $! checkUnitTypes "Sub" u1 u2 $ EvalReducedValue (Id $ Stats tp1 `Sub` Stats tp2) u1 (val1 - val2)
reduceBinaryOf Mult{} (EvalValue tp1 u1 _ _ val1) (EvalValue tp2 u2 _ _ val2)       = force $! checkUnitTypes "Mult" u1 u2 $ EvalReducedValue (Id $ Stats tp1 `Mult` Stats tp2) u1 (val1 * val2)
reduceBinaryOf Div{}  (EvalValue tp1 u1 _ _ val1) (EvalValue tp2 u2 _ _ val2)       = force $! checkUnitTypes "Div" u1 u2 $ EvalReducedValue (Id $ Stats tp1 `Div` Stats tp2) u1 (val1 / val2)
reduceBinaryOf Add{}  (EvalReducedValue tp1 u1 val1) (EvalReducedValue tp2 u2 val2) = force $! checkUnitTypes "Add" u1 u2 $ EvalReducedValue (Id $ Stats tp1 `Add` Stats tp2) u1 (val1 + val2)
reduceBinaryOf Sub{}  (EvalReducedValue tp1 u1 val1) (EvalReducedValue tp2 u2 val2) = force $! checkUnitTypes "Sub" u1 u2 $ EvalReducedValue (Id $ Stats tp1 `Sub` Stats tp2) u1 (val1 - val2)
reduceBinaryOf Mult{} (EvalReducedValue tp1 u1 val1) (EvalReducedValue tp2 u2 val2) = force $! checkUnitTypes "Mult" u1 u2 $ EvalReducedValue (Id $ Stats tp1 `Mult` Stats tp2) u1 (val1 * val2)
reduceBinaryOf Div{}  (EvalReducedValue tp1 u1 val1) (EvalReducedValue tp2 u2 val2) = force $! checkUnitTypes "Div" u1 u2 $ EvalReducedValue (Id $ Stats tp1 `Div` Stats tp2) u1 (val1 / val2)
reduceBinaryOf Add{}  (EvalValue tp1 u1 _ _ val1) (EvalReducedValue tp2 u2 val2)    = force $! checkUnitTypes "Add" u1 u2 $ EvalReducedValue (Id $ Stats tp1 `Add` Stats tp2) u1 (val1 + val2)
reduceBinaryOf Sub{}  (EvalValue tp1 u1 _ _ val1) (EvalReducedValue tp2 u2 val2)    = force $! checkUnitTypes "Sub" u1 u2 $ EvalReducedValue (Id $ Stats tp1 `Sub` Stats tp2) u1 (val1 - val2)
reduceBinaryOf Mult{} (EvalValue tp1 u1 _ _ val1) (EvalReducedValue tp2 u2 val2)    = force $! checkUnitTypes "Mult" u1 u2 $ EvalReducedValue (Id $ Stats tp1 `Mult` Stats tp2) u1 (val1 * val2)
reduceBinaryOf Div{}  (EvalValue tp1 u1 _ _ val1) (EvalReducedValue tp2 u2 val2)    = force $! checkUnitTypes "Div" u1 u2 $ EvalReducedValue (Id $ Stats tp1 `Div` Stats tp2) u1 (val1 / val2)
-- reduceBinaryOf Add{}  (EvalValue tp1 u1 _ _ val1) (EvalReducedValue tp2 u2 val2)    = force $ checkUnitTypes "Add" u1 u2 $ EvalReducedValue (Id $ Stats tp1 `Add` Stats tp2) u1 (val1 + val2)
-- reduceBinaryOf Sub{}  (EvalValue tp1 u1 _ _ val1) (EvalReducedValue tp2 u2 val2)    = force $ checkUnitTypes "Sub" u1 u2 $ EvalReducedValue (Id $ Stats tp1 `Sub` Stats tp2) u1 (val1 - val2)
-- reduceBinaryOf Mult{} (EvalValue tp1 u1 _ _ val1) (EvalReducedValue tp2 u2 val2)    = force $ checkUnitTypes "Mult" u1 u2 $ EvalReducedValue (Id $ Stats tp1 `Mult` Stats tp2) u1 (val1 * val2)
-- reduceBinaryOf Div{}  (EvalValue tp1 u1 _ _ val1) (EvalReducedValue tp2 u2 val2)    = force $ checkUnitTypes "Div" u1 u2 $ EvalReducedValue (Id $ Stats tp1 `Div` Stats tp2) u1 (val1 / val2)

-- Todo: reduce with EvalVector?
reduceBinaryOf eval _ _ = error $ "unexpected binary reduce: " ++ show eval


checkUnitTypes :: (Eq a, Show a) => String -> a -> a -> p -> p
checkUnitTypes f a b c | a == b = c
                       | otherwise = error $ f <> " on different units: " <> show a <> ", " <> show b
