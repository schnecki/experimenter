{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}


module Main where

import           Control.DeepSeq
import           Data.Serialize
import           GHC.Generics
import           System.Random

import           Experimenter


data Dice =
  Dice
    StdGen -- ^ Random number generator
    Int    -- ^ Nr of sides of the dice. E.g. 6 sided dice.
  deriving (Show, Generic)

instance NFData Dice where
  rnf (Dice !_ nr) = rnf nr

instance Serialize Dice where
  put (Dice g sides) = put (show g) >> put sides
  get = do
    g <- read <$> get
    sides <- get
    return $ Dice g sides

instance ExperimentDef Dice where
  type ExpM Dice = IO
  type InputValue Dice = ()
  type InputState Dice = ()
  type Serializable Dice = Dice

  -- ^ One step, that is one dice draw.
  runStep _ (Dice g sides) _ _ =
    let (nr, g') = next g
        result = StepResult "draw" Nothing (fromIntegral $ 1 + nr `mod` sides)
    in return ([result], Dice g' sides)

  -- ^ Parameters that are changed to get multiple experiment instances.
  parameters _ = [paramNrOfSides]


paramNrOfSides :: ParameterSetup Dice
paramNrOfSides =
  ParameterSetup
    "Numer of sides"
    (\sides (Dice g _) -> Dice g sides)  -- ^ Setter
    (\(Dice _ sides) -> sides)           -- ^ Getter
    (Just (\b -> return [b + 6, b - 6])) -- ^ A function on how to permute the parameter.
    (Just (6, 30))                       -- ^ Parameter limits, e.g. the parameter value cannot grow below/above this value.
    Nothing                              -- ^ Flag to indicate skipping of the preparation phase.
    Nothing                              -- ^ Function to define the ExperimentDesign. E.g. maybe the results do not depend on other parameters, then only one run is executed.

setup :: MkExperimentSetting a
setup _ =
  ExperimentSetting
    { _experimentBaseName             = "dice param experiment" -- ^ Gives the experiment a name.
    , _experimentInfoParameters       = [info]                  -- ^ For storing information which only effects the report.
    , _experimentRepetitions          = 2                       -- ^ How often to repeat the whole experiment (including the preperation phase).
    , _preparationSteps               = 0                       -- ^ How many steps to execute for the preperation phase.
    , _evaluationWarmUpSteps          = 1000                    -- ^ How many steps to execute for the warm-up phase.
    , _evaluationSteps                = 10000                   -- ^ How many steps to execute for the evaluation phase.
    , _evaluationReplications         = 1                       -- ^ How often to execute the evaluation for each experiment repetition.
    , _evaluationMaxStepsBetweenSaves = Just 100000             -- ^ Specify after how many steps the data will be saved. `Nothing` adaptively chooses a sensible value.
    }
  where
    info =
      ExperimentInfoParameter
        "This is an example info parameter"
        ("It is used to check if the cell is correctly split into multiple lines. Here you might want to store more useful information" :: String)


main :: IO ()
main = do
  g <- newStdGen
  let databaseSetup = DatabaseSetting "host=localhost dbname=experimenter2 user=experimenter password= port=5432" 10
  let initState = Dice g 6
  (changed, res) <- runExperimentsIO databaseSetup setup () initState
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
