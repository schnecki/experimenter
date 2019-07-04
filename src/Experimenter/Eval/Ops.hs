{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Experimenter.Eval.Ops
    ( genEvals
    , genEvalsIO
    ) where

import           Control.Lens                hiding (Cons, Over, over)
import           Control.Monad               (unless)
import           Control.Monad.Logger        (logDebug)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Function               (on)
import           Data.List                   (find, sortBy)
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import           Database.Persist.Postgresql (SqlBackend, runSqlConn, withPostgresqlConn)

import           Experimenter.DatabaseSetup
import           Experimenter.Eval.Reduce
import           Experimenter.Eval.Type      as E
import           Experimenter.Experiment
import           Experimenter.Measure
import           Experimenter.Result.Type
import           Experimenter.StepResult
import           Experimenter.Util


makeResDataAvailable :: (ExperimentDef a) => Experiments a -> ReaderT SqlBackend (LoggingT (ExpM a)) (Experiments a)
makeResDataAvailable exps =
  (experiments . traversed . experimentResults . traversed . evaluationResults . traversed . evalResults . traversed . results) mkAvailable exps >>=
  mapMOf (experiments . traversed . experimentResults . traversed . evaluationResults . traversed . evalResults . traversed . inputValues) mkAvailable
  -- mapMOf (experiments . traversed . experimentResults . traversed . warmUpResults . traversed . evalResults . traversed . results) mkAvailable >>=
  -- mapMOf (experiments . traversed . experimentResults . traversed . warmUpResults . traversed . evalResults . traversed . inputValues) mkAvailable >>=
  -- mapMOf (experiments . traversed . preparationResults . traversed . evalResults . traversed . results) mkAvailable >>=
  -- mapMOf (experiments . traversed . preparationResults . traversed . evalResults . traversed . inputValues) mkAvailable
  where
    mkAvailable (Available xs)          = return $ Available xs
    mkAvailable (AvailableFromDB query) = Available <$> query

runner :: (ExperimentDef a) => (ExpM a (Experiments a) -> IO (Experiments a)) -> DatabaseSetup -> Experiments a -> IO (Experiments a)
runner runExpM dbSetup exps =
  runExpM $
  (runStdoutLoggingT . filterLogger (\s _ -> s /= "SQL")) $ withPostgresqlConn (connectionString dbSetup) $ \backend -> flip runSqlConn backend $ makeResDataAvailable exps


genEvalsIO :: (ExperimentDef a, IO ~ ExpM a) => DatabaseSetup -> Experiments a -> [StatsDef a] -> IO (Evals a)
genEvalsIO = genEvals id

genEvals :: (ExperimentDef a) => (ExpM a (Experiments a) -> IO (Experiments a)) -> DatabaseSetup -> Experiments a -> [StatsDef a] -> IO (Evals a)
genEvals runExpM dbSetup exps evals = do
  exps' <- runner runExpM dbSetup exps
  res <- mapM mkEval (exps' ^. experiments)
  return $ Evals exps' res
  where
    mkEval e = do
      xs <- mapM (genExperiment e) evals
      return $ ExperimentEval (e ^. experimentNumber) xs e

genExperiment :: Experiment a -> StatsDef a -> IO (EvalResults a)
genExperiment exp (Named eval name) = addName <$> genExperiment exp eval
  where addName res = res { _evalType = Named (res ^. evalType) name }
genExperiment exp eval =
  case eval of
    Mean OverExperimentRepetitions eval' -> reduce eval' <$> genExpRes id (Id eval')
    Sum OverExperimentRepetitions eval' -> reduce eval' <$> genExpRes id (Id eval')
    StdDev OverExperimentRepetitions eval' -> reduce eval' <$> genExpRes id (Id eval')
    Mean (OverBestXExperimentRepetitions nr cmp) eval' -> reduce eval' <$> genExpRes (take nr . sortBy (cmp `on` id)) (Id eval')
    Sum (OverBestXExperimentRepetitions nr cmp) eval' -> reduce eval' <$> genExpRes (take nr . sortBy (cmp `on` id)) (Id eval')
    StdDev (OverBestXExperimentRepetitions nr cmp) eval' -> reduce eval' <$> genExpRes (take nr . sortBy (cmp `on` id)) (Id eval')
    _ -> packGenRes <$> genExpRes id eval
  where
    -- packGenRes [x] = x
    packGenRes xs  = EvalVector eval UnitExperimentRepetition xs
    genExpRes f e = mapM (genExperimentResult exp e) (f $ exp ^. experimentResults)
    reduce eval' = reduceUnary eval . EvalVector (Id eval') UnitExperimentRepetition


genExperimentResult :: Experiment a -> StatsDef a -> ExperimentResult a -> IO (EvalResults a)
genExperimentResult _ (Named _ n) _ = error $ "An evaluation may only be named on the outermost function in evaluation " <> T.unpack n
genExperimentResult exp eval expRes =
  case eval of
    Mean OverReplications eval'   -> reduce <$> genRepl (Id eval')
    StdDev OverReplications eval' -> reduce <$> genRepl (Id eval')
    Sum OverReplications eval'    -> reduce <$> genRepl (Id eval')
    _                             -> packGenRes <$> genRepl eval
    -- packGenRes [x] = x
  where
    packGenRes xs = EvalVector eval UnitReplications xs
    genRepl e = mapM (genReplication exp e) (expRes ^. evaluationResults)
    reduce inp = EvalVector eval (getUnit inp) $ map (reduceUnary eval)  $ transpose UnitReplications inp
    getUnit (EvalVector _ unit _:_)       = unit
    getUnit (EvalValue _ unit _ _ _:_)    = unit
    getUnit (EvalReducedValue _ unit _:_) = unit


genReplication :: Experiment a -> StatsDef a -> ReplicationResult a -> IO (EvalResults a)
genReplication exp eval repl = fromMaybe (error "Evaluation data is incomplete!") <$> sequence (genResultData exp eval <$> (repl ^. evalResults))


genResultData :: Experiment a -> StatsDef a -> ResultData a -> IO (EvalResults a)
genResultData _ (Named _ n) _ = error $ "An evaluation may only be named on the outermost function in evaluation " <> T.unpack n
genResultData exp eval repl =
  case eval of
    Mean OverPeriods eval'   -> reduceUnary eval <$> genResultData exp (Id eval') repl
    StdDev OverPeriods eval' -> reduceUnary eval <$> genResultData exp (Id eval') repl
    Sum OverPeriods eval'    -> reduceUnary eval <$> genResultData exp (Id eval') repl
    Id eval'                 -> evalOf exp eval' repl
    _                        -> genExperiment exp eval


fromAvailable :: Availability a b -> b
fromAvailable (Available b) = b
fromAvailable (AvailableFromDB _) = error "Data was not loaded from DB. Need to load data before running the evaluation!"

evalOf :: Experiment a -> Of a -> ResultData a -> IO (EvalResults a)
evalOf exp eval resData =
  case eval of
    Of name              -> return $ EvalVector (Id $ Of name) UnitPeriods $  sortBy (compare `on` (^?! evalX)) $ map (fromMeasure name) (fromAvailable $ resData ^. results)
    Stats def            -> genExperiment exp def
    Div eval1 eval2      -> reduceBinaryOf eval <$> evalOf exp eval1 resData <*> evalOf exp eval2 resData
    Add eval1 eval2      -> reduceBinaryOf eval <$> evalOf exp eval1 resData <*> evalOf exp eval2 resData
    Sub eval1 eval2      -> reduceBinaryOf eval <$> evalOf exp eval1 resData <*> evalOf exp eval2 resData
    Mult eval1 eval2     -> reduceBinaryOf eval <$> evalOf exp eval1 resData <*> evalOf exp eval2 resData
    First eval'          -> reduceUnaryOf eval <$> evalOf exp eval' resData
    Last eval'           -> reduceUnaryOf eval <$> evalOf exp eval' resData
    EveryXthElem _ eval' -> reduceUnaryOf eval <$> evalOf exp eval' resData
    Length eval'         -> reduceUnaryOf eval <$> evalOf exp eval' resData

fromMeasure :: T.Text -> Measure -> EvalResults a
fromMeasure name (Measure p res) = case find ((==name) . view resultName) res of
  Nothing -> error $ "Variable with name " <> T.unpack name <> " could not be found!"
  Just (StepResult n mX y) -> EvalValue (Id $ Of n) UnitPeriods n (maybe (Left p) Right mX) y

