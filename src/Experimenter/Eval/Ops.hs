{-# LANGUAGE OverloadedStrings #-}

module Experimenter.Eval.Ops
    ( genEvals
    ) where

import           Control.Lens             hiding (Cons, over)
import           Data.Function            (on)
import           Data.List                (find, sortBy)
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T

import           Experimenter.Eval.Reduce
import           Experimenter.Eval.Type
import           Experimenter.Measure
import           Experimenter.Result.Type
import           Experimenter.StepResult

-- writeEval :: EvalTable a -> IO ()
-- writeEval = undefined

genEvals :: Experiments a -> [Evaluation a] -> IO (Evals a)
genEvals exps evals = do
  res <- mapM (\e -> mapM (genExperiment e) evals >>= return . ExperimentEval (e ^. experimentNumber)) (exps ^. experiments)
  return $ Evals exps res


genExperiment :: Experiment a -> Evaluation a -> IO (EvalResults a)
genExperiment exp eval = case eval of
    MeanOver (BestXExperimentEvaluations nr cmp) eval' -> reduceUnary eval . EvalVector UnitExperiments <$> genExpRes (take nr . sortBy (cmp `on` id)) eval'
    SumOver (BestXExperimentEvaluations nr cmp) eval' ->  reduceUnary eval . EvalVector UnitExperiments <$> genExpRes (take nr . sortBy (cmp `on` id)) eval'
    StdDevOver (BestXExperimentEvaluations nr cmp) eval' ->  reduceUnary eval . EvalVector UnitExperiments <$> genExpRes (take nr . sortBy (cmp `on` id)) eval'
    _ -> EvalVector UnitExperiments <$> genExpRes id eval
  where genExpRes f e = mapM (genExperimentResult e) (f $ exp ^. experimentResults)


genExperimentResult :: (Evaluation a) -> ExperimentResult a -> IO (EvalResults a)
genExperimentResult eval expRes =
  case eval of
    MeanOver Replications eval' -> map (reduceOver eval) <$> genRepl eval'
    StdDevOver Replications eval' -> map (reduceOver eval) <$> genRepl eval'
    SumOver Replications eval' -> map (reduceOver eval) <$> genRepl eval'
    _ -> concat <$> mapM (genReplication eval) (expRes ^. evaluationResults)
  where
    genRepl e = mapM (genReplication e) (expRes ^. evaluationResults)


genReplication :: Evaluation a -> ReplicationResult a -> IO (EvalResults a)
genReplication eval repl = fromMaybe (error "Evaluation data is incomplete!") <$> sequence (genResultData eval <$> (repl ^. evalResults))


genResultData :: Evaluation a -> ResultData a -> IO (EvalResults a)
genResultData eval repl = case eval of
  MeanOver Periods eval'   -> reduceUnary eval <$> genResultData eval' repl
  StdDevOver Periods eval' -> reduceUnary eval <$> genResultData eval' repl
  SumOver Periods eval'    -> reduceUnary eval <$> genResultData eval' repl
  Div eval1 eval2          -> do
    e1 <- genResultData eval1 repl
    e2 <- genResultData eval2 repl
    return $ reduceBinary eval e1 e2
  Of valName               -> return $ EvalVector Periods (map (fromMeasure valName) (repl ^. results))


fromMeasure :: T.Text -> Measure -> (EvalResults a)
fromMeasure name (Measure p res) = case find ((==name) . view resultName) res of
  Nothing -> error $ "Variable with name " <> T.unpack name <> " could not be found!"
  Just (StepResult n mX y) -> EvalValue (Of n) n (maybe (Left p) Right mX) y

