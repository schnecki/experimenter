{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}


module Main where

import           Control.Lens   (view)
import           Data.Ratio
import           Data.Serialize
import qualified Data.Text      as T
import           GHC.Generics
import           System.Random

import           Experimenter


data Dice = Dice (Maybe Double)
  deriving (Show, Generic)

instance Serialize Dice where
  put (Dice mD) = put mD
  get = do
    mD <- get
    return $ Dice mD

instance ExperimentDef Dice where
  type InputValue Dice = StdGen
  type InputState Dice = ()
  type Serializable Dice = Dice
  serialisable = id
  deserialisable = id

  generateInput g _ _ _ = return (g, ())
  runStep (Dice mD) g _ =
    let (nr, _) = next g
        result = StepResult "draw" Nothing (fromIntegral $ 1 + nr `mod` 6)
    in return ([result], Dice mD)
  parameters _ = [fakeParam]

  -- Either compare params or do not compare them, but for sure do not compare random generators! As they are never
  -- equal!
  equalExperiments (Dice f1,_) (Dice f2,_) = f1 == f2
  equalExperiments _ _                     = True


fakeParam :: ParameterSetup Dice
fakeParam = ParameterSetup "fake" (\mD (Dice _) -> Dice mD) (\(Dice mD) -> mD) (Just (\(Just b) -> return [Just (fromR $ toR b - 0.2), Just (fromR $ toR b + 0.2)])) (Just 0, Just 0.4)
  where
    toR x = approxRational x 0.0001
    fromR = fromRational


setup :: ExperimentSetup
setup = ExperimentSetup
  { _experimentBaseName         = "dice param 3"
  , _experimentRepetitions      =  2
  , _preparationSteps           =  0
  , _evaluationWarmUpSteps      =  0
  , _evaluationSteps            =  10
  , _evaluationReplications     =  3
  , _maximumParallelEvaluations =  1
  }


main :: IO ()
main = do
  let databaseSetup = DatabaseSetup "host=localhost dbname=experimenter user=schnecki password= port=5432" 10
  (changed, res) <- runExperimentsLoggingNoSql databaseSetup setup () (Dice (Just 0.2))
  putStrLn $ "Any change: " ++ show changed
  let evals = [ Mean OverExperimentRepetitions (Of "draw"), StdDev OverExperimentRepetitions (Of "draw")
              , Mean OverReplications (Of "draw"), StdDev OverReplications (Of "draw")
              , Id (Of "draw"), Id (Of "draw")
              ]
  evalRes <- genEvals res evals
  print (view evalsResults evalRes)
  writeAndCompileLatex evalRes
