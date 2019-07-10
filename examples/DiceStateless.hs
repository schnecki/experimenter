{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Main where

import           Control.DeepSeq
import           Control.Lens        (view)
import           Data.Ratio
import           Data.Serialize
import qualified Data.Text           as T
import qualified Data.Vector.Unboxed as V
import           Data.Word           (Word32)
import           GHC.Generics
import           System.IO.Unsafe
import           System.Random.MWC

import           Experimenter


data Dice = Dice (Maybe Double)
  deriving (Show, Generic, NFData)

instance Serialize Dice where
  put (Dice mD) = put mD
  get = do
    mD <- get
    return $ Dice mD

instance Serialize Seed where
  put s = put $ fromSeed s
  get = do
    (vec :: V.Vector Word32) <- get
    return $ toSeed vec

instance ExperimentDef Dice where
  type ExpM Dice = IO
  type InputValue Dice = Seed
  type InputState Dice = ()
  type Serializable Dice = Dice
  serialisable = return
  deserialisable = return

  generateInput g _ _ _ = do
    seed <- save g
    return (seed, ())
  runStep (Dice mD) seed _ = do
    g <- restore seed
    (nr :: Int) <- uniformR (1,6) g
    let result = StepResult "draw" Nothing (fromIntegral $ 1 + nr `mod` 6)
    return ([result], Dice mD)
  parameters _ = [fakeParam]

fakeParam :: ParameterSetup Dice
fakeParam = ParameterSetup "fake" (\mD (Dice _) -> Dice mD) (\(Dice mD) -> mD) (Just (\(Just b) -> return [Just (fromR $ toR b - 0.2), Just (fromR $ toR b + 0.2)])) (Just (Just 0, Just 0.4)) Nothing Nothing
  where
    toR x = approxRational x 0.0001
    fromR = fromRational


setup :: MkExperimentSetting a
setup _ = ExperimentSetting
  { _experimentBaseName         = "dice param"
  , _experimentInfoParameters = []
  , _experimentRepetitions      =  2
  , _preparationSteps           =  0
  , _evaluationWarmUpSteps      =  0
  , _evaluationSteps            =  10
  , _evaluationReplications     =  3
  , _maximumParallelEvaluations =  1
  }


main :: IO ()
main = do
  let databaseSetup = DatabaseSetting "host=localhost dbname=experimenter2 user=experimenter password= port=5432" 10
  (changed, res) <- runExperiments id databaseSetup setup () (Dice (Just 0.2))
  putStrLn $ "Any change: " ++ show changed
  let evals = [ Mean OverExperimentRepetitions (Of "draw"), StdDev OverExperimentRepetitions (Of "draw")
              , Mean OverReplications (Of "draw"), StdDev OverReplications (Of "draw")
              , Id (Of "draw"), Id (Of "draw")
              ]
  evalRes <- genEvalsIO databaseSetup res evals
  -- print (view evalsResults evalRes)
  writeAndCompileLatex evalRes
