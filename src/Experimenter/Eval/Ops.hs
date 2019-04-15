{-# LANGUAGE OverloadedStrings #-}

module Experimenter.Eval.Ops
    ( genEvals
    ) where

import           Control.Lens             hiding (Cons, Over, over)
import           Control.Monad            (unless)
import           Data.Function            (on)
import           Data.List                (find, sortBy)
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T

import           Experimenter.Eval.Reduce
import           Experimenter.Eval.Type   as E
import           Experimenter.Measure
import           Experimenter.Result.Type
import           Experimenter.StepResult

-- writeEval :: EvalTable a -> IO ()
-- writeEval = undefined

genEvals :: Experiments a -> [StatsDef a] -> IO (Evals a)
genEvals exps evals = do
  res <- mapM mkEval (exps ^. experiments)
  return $ Evals exps res
  where
    mkEval e = do
      xs <- mapM (genExperiment e) evals
      return $ ExperimentEval (e ^. experimentNumber) xs

genExperiment :: Experiment a -> StatsDef a -> IO (EvalResults a)
genExperiment exp eval =
  case eval of
    Mean OverExperiments eval' -> reduce eval' <$> genExpRes id (Id eval')
    Sum OverExperiments eval' -> reduce eval' <$> genExpRes id (Id eval')
    StdDev OverExperiments eval' -> reduce eval' <$> genExpRes id (Id eval')
    Mean (OverBestXExperimentEvaluations nr cmp) eval' -> reduce eval' <$> genExpRes (take nr . sortBy (cmp `on` id)) (Id eval')
    Sum (OverBestXExperimentEvaluations nr cmp) eval' -> reduce eval' <$> genExpRes (take nr . sortBy (cmp `on` id)) (Id eval')
    StdDev (OverBestXExperimentEvaluations nr cmp) eval' -> reduce eval' <$> genExpRes (take nr . sortBy (cmp `on` id)) (Id eval')
    _ -> EvalVector eval UnitExperiments <$> genExpRes id eval
  where
    genExpRes f e = mapM (genExperimentResult exp e) (f $ exp ^. experimentResults)
    reduce eval' = reduceUnary eval . EvalVector (Id eval') UnitExperiments


genExperimentResult :: Experiment a -> StatsDef a -> ExperimentResult a -> IO (EvalResults a)
genExperimentResult exp eval expRes =
  case eval of
    Mean OverReplications eval'   -> reduce eval' <$> genRepl (Id eval')
    StdDev OverReplications eval' -> reduce eval' <$> genRepl (Id eval')
    Sum OverReplications eval'    -> reduce eval' <$> genRepl (Id eval')
    _                             -> EvalVector eval UnitExperiments <$> genRepl eval
  where
    genRepl e = mapM (genReplication exp e) (expRes ^. evaluationResults)
    reduce eval' = reduceUnary eval . EvalVector (Id eval') UnitReplications


genReplication :: Experiment a -> StatsDef a -> ReplicationResult a -> IO (EvalResults a)
genReplication exp eval repl = fromMaybe (error "Evaluation data is incomplete!") <$> sequence (genResultData exp eval <$> (repl ^. evalResults))

genResultData :: Experiment a -> StatsDef a -> ResultData a -> IO (EvalResults a)
genResultData exp eval repl =
  case eval of
    Mean OverPeriods eval'   -> reduceUnary eval <$> genResultData exp (Id eval') repl
    StdDev OverPeriods eval' -> reduceUnary eval <$> genResultData exp (Id eval') repl
    Sum OverPeriods eval'    -> reduceUnary eval <$> genResultData exp (Id eval') repl
    Id eval'                 -> evalOf exp eval' repl
    _                        -> genExperiment exp eval


evalOf :: Experiment a -> Of a -> ResultData a -> IO (EvalResults a )
evalOf exp eval resData =
  case eval of
    Of name -> return $ EvalVector (Id $ Of name) UnitPeriods $ map (fromMeasure name) (resData ^. results)
    Stats def -> genExperiment exp def
    Div eval1 eval2 -> reduceBinary eval <$> evalOf exp eval1 resData <*> evalOf exp eval2 resData
    Add eval1 eval2 -> reduceBinary eval <$> evalOf exp eval1 resData <*> evalOf exp eval2 resData
    Sub eval1 eval2 -> reduceBinary eval <$> evalOf exp eval1 resData <*> evalOf exp eval2 resData
    Mult eval1 eval2 -> reduceBinary eval <$> evalOf exp eval1 resData <*> evalOf exp eval2 resData


fromMeasure :: T.Text -> Measure -> EvalResults a
fromMeasure name (Measure p res) = case find ((==name) . view resultName) res of
  Nothing -> error $ "Variable with name " <> T.unpack name <> " could not be found!"
  Just (StepResult n mX y) -> EvalValue (Id $ Of n) n (maybe (Left p) Right mX) y

