{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}


module Main where

import           Control.DeepSeq
import           Control.Lens    (view)
import           Data.Ratio
import           Data.Serialize
import qualified Data.Text       as T
import           GHC.Generics
import           System.Random

import           Experimenter


data Dice = Dice StdGen (Maybe Double)
  deriving (Show, Generic)

instance NFData Dice where
  rnf (Dice _ nr) = rnf nr

instance Serialize Dice where
  put (Dice g mD) = put (show g) >> put mD
  get = do
    g <- read <$> get
    mD <- get
    return $ Dice g mD


instance ExperimentDef Dice where
  type ExpM Dice = IO
  type InputValue Dice = ()
  type InputState Dice = ()
  type Serializable Dice = Dice
  serialisable = return
  deserialisable = return

  generateInput _ _ _ _ = return ((), ())
  runStep _ (Dice g mD) _ p =
    let (nr, g') = next g
        result = StepResult "draw" Nothing (fromIntegral $ 1 + nr `mod` 6)
    in return ([result], Dice g' mD)
  parameters _ = [] -- [fakeParam]

fakeParam :: ParameterSetup Dice
fakeParam = ParameterSetup "fake" (\mD (Dice g _) -> Dice g mD) (\(Dice _ mD) -> mD) (Just (\(Just b) -> return [Just (fromR $ toR b - 0.2), Just (fromR $ toR b + 0.2)])) (Just (Just 0, Just 1)) Nothing Nothing
  where
    toR x = approxRational x 0.0001
    fromR = fromRational


setup :: MkExperimentSetting a
setup _ = ExperimentSetting
  { _experimentBaseName         = "dice param"
  , _experimentInfoParameters = []
  , _experimentRepetitions      =  2
  , _preparationSteps           =  0
  , _evaluationWarmUpSteps      =  1000
  , _evaluationSteps            =  10000
  , _evaluationReplications     =  20
  , _maximumParallelEvaluations =  1
  }


main :: IO ()
main = do
  let databaseSetup = DatabaseSetting "host=localhost dbname=experimenter2 user=experimenter password= port=5432" 10
  g <- newStdGen
  (changed, res) <- runExperimentsIO databaseSetup setup () (Dice g (Just 0.2))
  putStrLn $ "Any change: " ++ show changed
  let evals = [ Mean OverExperimentRepetitions (Of "draw") `Named` "Mean Repetitions"
              , StdDev OverExperimentRepetitions (Of "draw") `Named` "StdDev Repetitions"

              , Mean OverReplications (EveryXthElem 100 $ Of "draw") `Named` "Mean Repls1"
              , StdDev OverReplications (EveryXthElem 100 $ Of "draw") `Named` "StdDev Repls"

              -- not allowed as OverExperimentRepetitions and OverReplications have to be the outermost elements!
              -- , Id (EveryXthElem 100 (Stats $ Mean OverReplications (Of "draw"))) `Named` "Mean Repls2"

              , Id (EveryXthElem 100 (Of "draw")) `Named` "draws 1"
              , Id (EveryXthElem 100 (Of "draw")) `Named` "draws 2"
              ]
  evalRes <- genEvalsIO databaseSetup res evals
  -- print (view evalsResults evalRes)
  writeAndCompileLatex databaseSetup evalRes
  return ()
