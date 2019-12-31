{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}


module Experimenter.Eval.Ops
    ( genEvals
    , genEvalsIO
    ) where

import           Control.DeepSeq
import           Control.Lens                 hiding (Cons, Over, over)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Function                (on)
import           Data.List                    (find, sortBy)
import           Data.Maybe                   (fromMaybe)
import           Data.Serialize               as S
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as E
import           Data.Time                    (addUTCTime, getCurrentTime)
import           Database.Persist.Postgresql  as DB (SqlBackend, delete, get, insert,
                                                     runSqlConn, selectKeysList,
                                                     withPostgresqlConn, (<=.))
import           Database.Persist.Sql         (fromSqlKey, toSqlKey)


import           Experimenter.Availability
import           Experimenter.DatabaseSetting
import           Experimenter.DB
import           Experimenter.Eval.Reduce
import           Experimenter.Eval.Type       as E
import           Experimenter.Experiment
import           Experimenter.Measure
import           Experimenter.Models
import           Experimenter.Result.Type
import           Experimenter.StepResult


genEvalsIO :: (ExperimentDef a, IO ~ ExpM a) => DatabaseSetting -> Experiments a -> [StatsDef a] -> IO (Evals a)
genEvalsIO = genEvals id

genEvals :: (ExperimentDef a) => (ExpM a (Evals a) -> IO (Evals a)) -> DatabaseSetting -> Experiments a -> [StatsDef a] -> IO (Evals a)
genEvals runExpM dbSetup exps evals = runExpM $ runDBWithM runResourceT dbSetup $ runner exps evals


runner :: (ExperimentDef a) => Experiments a -> [StatsDef a] -> DB (ExpM a) (Evals a)
runner exps evals = do
  -- Delete old temporary data
  time <- liftIO getCurrentTime
  selectKeysList [EvalResultTime <=. addUTCTime (-60*60*24) time] [] >>= mapM_ delete
  -- Evaluate and get new data
  !(force -> res) <- mapMRnf mkEval (exps ^. experiments)
  return $ Evals (exps {_experiments = []}) res
  where
    mkEval e = do
      -- !(force -> xs) <- mapM (genExperiment e >=> fmap force . saveEvalResults) evals
      !(force -> xs) <- mapMRnf (genExperiment e >=> fmap force . saveEvalResults) evals
      return $ force $ ExperimentEval (e ^. experimentNumber) xs (e { _experimentResults = []})

mapMRnf :: (NFData b, Monad m) => (a -> m b) -> [a] -> m [b]
mapMRnf _ [] = return []
mapMRnf f (x:xs) = do
  !(force -> x') <- f x
  (x' :) <$!!> mapMRnf f xs

saveEvalResults :: (ExperimentDef a) => EvalResults a -> DB (ExpM a) (Availability IO (EvalResults a))
saveEvalResults ev = do
  let bs = runPut (put ev)
  time <- liftIO getCurrentTime
  eId <- insert $ EvalResult time bs
  return $
    AvailableOnDemand $ do
      mEvalRes <- DB.get eId
      return $
        case mEvalRes of
          Nothing -> error $ "Could not get the eval data from the DB. Key " ++ show (fromSqlKey eId)
          Just (EvalResult _ bsDb) ->
            case runGet S.get bsDb of
              Left err -> error $ "Could not deserialise eval data: " <> err
              Right x  -> x


genExperiment :: (ExperimentDef a) => Experiment a -> StatsDef a -> DB (ExpM a) (EvalResults a)
genExperiment exp (Named eval name) = addName <$!!> genExperiment exp eval
  where addName res = res { _evalType = Named (res ^. evalType) name }
genExperiment exp eval =
  case eval of
    Mean OverExperimentRepetitions eval'   -> reduce eval' <$!!> genExpRes id (Id eval')
    Sum OverExperimentRepetitions eval'    -> reduce eval' <$!!> genExpRes id (Id eval')
    StdDev OverExperimentRepetitions eval' -> reduce eval' <$!!> genExpRes id (Id eval')
    -- Mean (OverBestXExperimentRepetitions nr cmp) eval' -> reduce eval' <$> genExpRes (take nr . sortBy (cmp `on` id)) (Id eval')
    -- Sum (OverBestXExperimentRepetitions nr cmp) eval' -> reduce eval' <$> genExpRes (take nr . sortBy (cmp `on` id)) (Id eval')
    -- StdDev (OverBestXExperimentRepetitions nr cmp) eval' -> reduce eval' <$> genExpRes (take nr . sortBy (cmp `on` id)) (Id eval')
    _                                      -> packGenRes <$!!> genExpRes id eval
  where
    -- packGenRes [x] = x
    packGenRes xs  = EvalVector eval UnitExperimentRepetition xs
    genExpRes f e = mapMRnf (fmap force . genExperimentResult exp e) (f $ exp ^. experimentResults)
    reduce eval' = reduceUnary eval . EvalVector (Id eval') UnitExperimentRepetition


genExperimentResult :: (ExperimentDef a) => Experiment a -> StatsDef a -> ExperimentResult a -> DB (ExpM a) (EvalResults a)
genExperimentResult _ (Named _ n) _ = error $ "An evaluation may only be named on the outermost function in evaluation " <> T.unpack (E.decodeUtf8 n)
genExperimentResult exp eval expRes =
  case eval of
    Mean OverReplications eval'   -> reduce <$!!> genRepl (Id eval')
    StdDev OverReplications eval' -> reduce <$!!> genRepl (Id eval')
    Sum OverReplications eval'    -> reduce <$!!> genRepl (Id eval')
    _                             -> packGenRes <$!!> genRepl eval
  where
    packGenRes = EvalVector eval UnitReplications
    genRepl e = mapMRnf (fmap force . genReplication exp e) (expRes ^. evaluationResults)
    reduce !inp = force $! EvalVector eval (getUnit inp) $ map (reduceUnary eval) $ transpose UnitReplications inp
    getUnit (EvalVector _ unit _:_) = unit
    getUnit (EvalValue _ unit _ _ _:_) = unit
    getUnit (EvalReducedValue _ unit _:_) = unit
    getUnit [] = error "Unexpected empty data in getUnit in genExperimentResult in Eval.Ops"


genReplication :: (ExperimentDef a) => Experiment a -> StatsDef a -> ReplicationResult a -> DB (ExpM a) (EvalResults a)
genReplication exp eval repl = fromMaybe (error "Evaluation data is incomplete!") <$!!> sequence (fmap force . genResultData exp eval <$!> (repl ^. evalResults))


genResultData :: (ExperimentDef a) => Experiment a -> StatsDef a -> ResultData a -> DB (ExpM a) (EvalResults a)
genResultData _ (Named _ n) _ = error $ "An evaluation may only be named on the outermost function in evaluation " <> T.unpack (E.decodeUtf8 n)
genResultData exp eval repl =
  case eval of
    Mean OverPeriods eval'   -> reduceUnary eval <$!!> genResultData exp (Id eval') repl
    StdDev OverPeriods eval' -> reduceUnary eval <$!!> genResultData exp (Id eval') repl
    -- Sum OverPeriods of'@Of{} -> reduceUnary eval <$!!> evalOf exp of' repl
    Sum OverPeriods eval'    -> reduceUnary eval <$!!> genResultData exp (Id eval') repl
    Id eval'                 -> force <$!> evalOf exp eval' repl
    _                        -> force <$!> genExperiment exp eval


evalOf :: (ExperimentDef a) => Experiment a -> Of a -> ResultData a -> DB (ExpM a) (EvalResults a)
evalOf exp eval resData =
  case eval of
    Of name -> do
      !(force -> xs) <- mkTransientlyAvailableList (resData ^. results)
      return $ force $ EvalVector (Id $ Of name) UnitPeriods $ sortBy (compare `on` (^?! evalX)) $ map (fromMeasure $ E.decodeUtf8 name) xs
    Stats def -> genExperiment exp def
    Div eval1 eval2 -> reduceBinaryOf eval <$!!> evalOf exp eval1 resData <*> evalOf exp eval2 resData
    Add eval1 eval2 -> reduceBinaryOf eval <$!!> evalOf exp eval1 resData <*> evalOf exp eval2 resData
    Sub eval1 eval2 -> reduceBinaryOf eval <$!!> evalOf exp eval1 resData <*> evalOf exp eval2 resData
    Mult eval1 eval2 -> reduceBinaryOf eval <$!!> evalOf exp eval1 resData <*> evalOf exp eval2 resData
    First eval' -> reduceUnaryOf eval <$!!> evalOf exp eval' resData
    Last eval' -> reduceUnaryOf eval <$!!> evalOf exp eval' resData
    EveryXthElem _ eval' -> reduceUnaryOf eval <$!!> evalOf exp eval' resData
    Length eval' -> reduceUnaryOf eval <$!!> evalOf exp eval' resData

fromMeasure :: T.Text -> Measure -> EvalResults a
fromMeasure name (Measure p res) = case find ((==name) . view resultName) res of
  Nothing -> error $ "Variable with name " <> T.unpack name <> " could not be found!"
  Just (StepResult n mX y) -> EvalValue (Id $ Of $ E.encodeUtf8 n) UnitPeriods (E.encodeUtf8 n) (maybe (Left p) Right mX) y


