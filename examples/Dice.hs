{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}


module Main where


import           Data.Serialize
import qualified Data.Text      as T
import           GHC.Generics
import           System.Random

import           Experimenter


data Dice = Dice StdGen
  deriving (Show, Generic)

instance Serialize Dice where
  put (Dice g) = do
    put (show g)
  get = do
    gS <- get
    return $ Dice (read gS)


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


main :: IO ()
main = do
  let setup = ExperimentSetup "dice" 1 0 0 100 3 2
      databaseSetup = DatabaseSetup "host=localhost dbname=experimenter user=schnecki password= port=5432" 10
  g <- newStdGen
  (changed, res) <- runExperimentsLogging databaseSetup setup () (Dice g)
  putStrLn $ "Any change: " ++ show changed
