{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}


module Main where

import           Control.Lens   (view)
import           Data.Serialize
import qualified Data.Text      as T
import           GHC.Generics
import           System.Random

import           Experimenter


data Dice = Dice StdGen
  deriving (Show, Generic)

instance Serialize Dice where
  put (Dice g) = put (show g)
  get = Dice . read <$> get


instance ExperimentDef Dice where
  type InputValue Dice = ()
  type InputState Dice = ()
  generateInput _ _ _ _ = return ((), ())
  runStep (Dice g) _ p =
    let (nr, g') = next g
        result = StepResult "draw" (Just $ fromIntegral p) (fromIntegral $ 1 + nr `mod` 6)
    in return ([result], Dice g')
  parameters _ = []
  equalExperiments _ _ = True   -- Do not compare random generators!


setup :: ExperimentSetup
setup = ExperimentSetup
  { _experimentBaseName         = "dice"
  , _experimentRepetitions      =  3
  , _preparationSteps           =  0
  , _evaluationWarmUpSteps      =  0
  , _evaluationSteps            =  1000
  , _evaluationReplications     =  5
  , _maximumParallelEvaluations =  2
  }


main :: IO ()
main = do
  let databaseSetup = DatabaseSetup "host=localhost dbname=experimenter user=schnecki password= port=5432" 10
  g <- newStdGen
  (changed, res) <- runExperimentsLoggingNoSql databaseSetup setup () (Dice g)
  putStrLn $ "Any change: " ++ show changed

  let evals = [Mean OverReplications (Of "draw"), StdDev OverReplications (Of "draw")]
  evalRes <- genEvals res evals

  print (view evalsResults evalRes)
