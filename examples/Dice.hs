{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}


module Main where

import           Control.Lens   (view)
import           Data.Ratio
import           Data.Serialize
import qualified Data.Text      as T
import           GHC.Generics
import           System.Random

import           Experimenter


data Dice = Dice StdGen (Maybe Double)
  deriving (Show, Generic)

instance Serialize Dice where
  put (Dice g mD) = put (show g) >> put mD
  get = do
    g <- read <$> get
    mD <- get
    return $ Dice g mD


instance ExperimentDef Dice where
  type InputValue Dice = ()
  type InputState Dice = ()
  generateInput _ _ _ _ = return ((), ())
  runStep (Dice g mD) _ p =
    let (nr, g') = next g
        result = StepResult "draw" Nothing (fromIntegral $ 1 + nr `mod` 6)
    in return ([result], Dice g' mD)
  parameters _ = [fakeParam]

  -- Either compare params or do not compare them, but for sure do not compare random generators! As they are never
  -- equal!
  equalExperiments (Dice _ f1,_) (Dice _ f2,_) = f1 == f2
  equalExperiments _ _                         = True


fakeParam :: ParameterSetup Dice
fakeParam = ParameterSetup "fake" (\mD (Dice g _) -> Dice g mD) (\(Dice _ mD) -> mD) (Just (\(Just b) -> return [Just (fromR $ toR b - 0.2), Just (fromR $ toR b + 0.2)])) (Just 0, Just 1)
  where
    toR x = approxRational x 0.0001
    fromR = fromRational


setup :: ExperimentSetup
setup = ExperimentSetup
  { _experimentBaseName         = "dice param 9"
  , _experimentRepetitions      =  3
  , _preparationSteps           =  0
  , _evaluationWarmUpSteps      =  0
  , _evaluationSteps            =  100
  , _evaluationReplications     =  3
  , _maximumParallelEvaluations =  2
  }


main :: IO ()
main = do
  let databaseSetup = DatabaseSetup "host=localhost dbname=experimenter user=schnecki password= port=5432" 10
  g <- newStdGen
  (changed, res) <- runExperimentsLoggingNoSql databaseSetup setup () (Dice g (Just 0.2))
  putStrLn $ "Any change: " ++ show changed

  let evals = [Mean OverReplications (Of "draw"), StdDev OverReplications (Of "draw")
              -- ,
               -- Id (Of "draw")
              ]
  evalRes <- genEvals res evals

  print (view evalsResults evalRes)

  writeAndCompileLatex evalRes
