{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}

module Experimenter.Eval.Type where

import           Control.DeepSeq
import           Control.Lens              hiding (Over)
import           Data.ByteString           (ByteString)
import           Data.Serialize
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as E
import           GHC.Generics
import           Prelude                   hiding (sum)


import           Experimenter.Availability
import           Experimenter.Result.Type  hiding (Experiments)
import qualified Experimenter.Result.Type  as R

-- | Over datatype to reduce data vectors.

data Over a
  = OverPeriods
  | OverReplications
  | OverExperimentRepetitions
  -- implement this by sorting and taking/dropping results
  -- | OverBestXExperimentRepetitions Int (ExperimentResult a -> ExperimentResult a -> Ordering)
  deriving (Generic, Serialize, NFData)

instance Eq (Over a) where
  OverReplications == OverReplications = True
  OverPeriods == OverPeriods = True
  OverExperimentRepetitions == OverExperimentRepetitions = True
  -- OverBestXExperimentRepetitions _ _ == OverBestXExperimentRepetitions _ _ = True
  _ == _ = False

instance Show (Over a) where
  show OverReplications          = "Replications"
  show OverPeriods               = "Periods"
  show OverExperimentRepetitions = "Experiments"
  -- show (OverBestXExperimentRepetitions nr _) = "(BestXExperimentEvaluations " <> show nr <> ")"

instance Ord (Over a) where
  compare OverExperimentRepetitions OverExperimentRepetitions = EQ
  compare OverExperimentRepetitions _                         = GT
  compare OverReplications OverReplications                   = EQ
  compare OverReplications _                                  = GT
  compare OverPeriods OverPeriods                             = EQ
  compare OverPeriods _                                       = LT

  -- compare OverExperimentRepetitions OverReplications          = GT
  -- compare OverExperimentRepetitions OverPeriods               = GT

  -- compare OverReplications _                                  = LT
  -- compare OverReplications OverPeriods                        = GT
  -- compare OverPeriods OverPeriods                             = EQ
  -- compare OverPeriods _                                       = LT
  -- compare OverBestXExperimentRepetitions{} OverBestXExperimentRepetitions{} = EQ
  -- compare OverBestXExperimentRepetitions{} _                                = GT


-- | Definition of statisics. Is used to define the desired output.


data StatsDef a
  = Mean !(Over a) !(Of a)
  | StdDev !(Over a) !(Of a)
  | Sum !(Over a) !(Of a)
  --  | TakeBest Int (StatsDef a) (Of a)
  | Id !(Of a)
  | Named !(StatsDef a) !ByteString
  | Name !ByteString !(StatsDef a)
  deriving (Generic, Serialize, Show, Eq, Ord, NFData)

-- type Name =

getOver :: StatsDef a -> Maybe (Over a)
getOver (Mean o _)   = Just o
getOver (StdDev o _) = Just o
getOver (Sum o _)    = Just o
getOver (Id _)       = Nothing
getOver (Named _ _)  = Nothing
getOver (Name _ _)   = Nothing


data Of a
  = Of !ByteString
  | Stats !(StatsDef a)
  | Div !(Of a) !(Of a)
  | Add !(Of a) !(Of a)
  | Sub !(Of a) !(Of a)
  | Mult !(Of a) !(Of a)
  | Last !(Of a)
  | First !(Of a)
  | EveryXthElem !Int !(Of a)
  | Length !(Of a)
  deriving (Generic, Serialize, Show, Eq, Ord, NFData)

prettyStatsDef :: StatsDef a -> T.Text
prettyStatsDef statsDef =
  case statsDef of
    Named _ txt     -> E.decodeUtf8 txt
    Name txt _      -> E.decodeUtf8 txt
    Mean over of'   -> "Mean " <> prettyOver over <> " " <> prettyOf of'
    StdDev over of' -> "StdDev " <> prettyOver over <> " " <> prettyOf of'
    Sum over of'    -> "Sum " <> prettyOver over <> " " <> prettyOf of'
    Id (Of name)    -> E.decodeUtf8 name <> "s"
    Id of'          -> prettyOf of'

prettyOf :: Of a -> T.Text
prettyOf = dropDoublePars . prettyOf'
  where
    prettyOf' of' =
      case of' of
        Of name -> "of " <> E.decodeUtf8 name
        Stats statsDef -> "(" <> prettyStatsDef statsDef <> ")"
        Div x y -> "( " <> prettyOf' x <> ") / (" <> prettyOf' y <> ")"
        Add x y -> "( " <> prettyOf' x <> ") + (" <> prettyOf' y <> ")"
        Sub x y -> "( " <> prettyOf' x <> ") - (" <> prettyOf' y <> ")"
        Mult x y -> "( " <> prettyOf' x <> ") * (" <> prettyOf' y <> ")"
        Last x -> "Last(" <> prettyOf' x <> ")"
        First x -> "First(" <> prettyOf' x <> ")"
        Length x -> "Length(" <> prettyOf' x <> ")"
        EveryXthElem nr x -> "EveryXthElem(" <> T.pack (show nr) <> ", " <> prettyOf' x <> ")"
    dropDoublePars = T.replace "((" "(" . T.replace "))" ")"


prettyOver :: Over a -> T.Text
prettyOver ov = "over " <> case ov of
  OverPeriods               -> "periods"
  OverReplications          -> "replications"
  OverExperimentRepetitions -> "experiment repetitions"
  -- OverBestXExperimentRepetitions nr _ -> "best " <> T.pack (show nr) <> " experiment repetitions"


-- Helper functions for demoting StatsDefs to Ofs.

sum :: Over a -> Of a -> Of a
sum over of' = Stats (Sum over of')

stdDev :: Over a -> Of a -> Of a
stdDev over of' = Stats (StdDev over of')

mean :: Over a -> Of a -> Of a
mean over of' = Stats (Mean over of')


-- | Simple examples on how to use the types

example :: StatsDef a
example = Mean OverReplications (sum OverPeriods (Of "NrEarly") `Div` sum OverPeriods (Of "NrOrders"))

example2 :: StatsDef a
example2 = Mean OverReplications (Of "NrEarly" `Div` Of "NrOrders")


example3 :: StatsDef a
example3 = Mean OverReplications (Of "X")


-- | Datatypes for the evaluation result.

data Unit
  = UnitPeriods
  | UnitReplications
  | UnitExperimentRepetition
  --  | UnitBestExperimentRepetitions Int
  | UnitScalar
  deriving (Generic, Serialize, Read, Eq, Ord, Show, NFData)

data EvalResults a
  = EvalVector { _evalType   :: !(StatsDef a)
               , _evalUnit   :: !Unit -- ^ Over which the vector runs.
               , _evalValues :: ![EvalResults a]
               }
  | EvalValue { _evalType         :: !(StatsDef a)
              , _evalUnit         :: !Unit -- ^ Is always periods.
              , _evalVariableName :: !ByteString
              , _evalX            :: !(Either Int Double) -- ^ Either period or xValue.
              , _evalY            :: !Double }
  | EvalReducedValue { _evalType  :: !(StatsDef a)
                     , _evalUnit  :: !Unit -- ^ Over which was reduced.
                     , _evalValue :: !Double }
  deriving (Generic, Serialize, Show, NFData)
makeLenses ''EvalResults

data ExperimentEval a = ExperimentEval
  { _evalExperimentNumber  :: !Int
  , _evalExperimentResults :: ![Availability IO (EvalResults a)]
  , _evalExperiment        :: !(Experiment a)
  } deriving (Generic, NFData)
makeLenses ''ExperimentEval

instance Show (ExperimentEval a) where
  show x = show (x ^. evalExperimentResults)

data Evals a = Evals
  { _evalsExperiments :: !(R.Experiments a)
  , _evalsResults     :: ![ExperimentEval a]
  } deriving (Generic, NFData)
makeLenses ''Evals


-- Helper Functions

getEvalValue :: EvalResults a -> [Double]
getEvalValue (EvalVector _ _ xs)      = concatMap getEvalValue xs
getEvalValue (EvalValue _ _ _ _ y)    = [y]
getEvalValue (EvalReducedValue _ _ y) = [y]

getEvalType :: (Over a -> Of a -> StatsDef a) -> EvalResults a -> StatsDef a
getEvalType f (EvalVector tp unit _)     = f (fromUnit unit) (Stats tp)
  where fromUnit UnitPeriods              = OverPeriods
        fromUnit UnitReplications         = OverReplications
        fromUnit UnitExperimentRepetition = OverExperimentRepetitions
        -- fromUnit (UnitBestExperimentRepetitions nr) = OverBestXExperimentRepetitions nr (error "compare function in BestXExperimentEvaluations may not be used")
        fromUnit UnitScalar               = OverExperimentRepetitions -- TODO really?
getEvalType _ (EvalValue t _ _ _ _)      = t
getEvalType _ (EvalReducedValue t _ _) = t

fromOver :: Over a -> Unit
fromOver OverPeriods               = UnitPeriods
fromOver OverReplications          = UnitReplications
fromOver OverExperimentRepetitions = UnitExperimentRepetition
-- fromOver (OverBestXExperimentRepetitions nr _) = UnitBestExperimentRepetitions nr


-- | Demotes the unit by 1 degree. Thus this calculates the unit of a vector over which it was reduced.
demoteUnit :: Unit -> Maybe Unit
demoteUnit UnitPeriods              = Nothing
demoteUnit UnitReplications         = Just UnitPeriods
demoteUnit UnitExperimentRepetition = Just UnitReplications
-- demoteUnit UnitBestExperimentRepetitions{} = Just UnitReplications
demoteUnit UnitScalar               = Just UnitExperimentRepetition
