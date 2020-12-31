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
    , genEvalsConcurrent
    ) where

import           Conduit                      as C
import           Control.DeepSeq
import           Control.Lens                 hiding (Cons, Over)
import           Control.Monad.Reader
import qualified Data.Conduit.List            as CL
import           Data.List                    (find)
import           Data.Maybe                   (fromMaybe, isJust)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as E
import qualified Database.Esqueleto           as E
import           Prelude                      hiding (exp)


import           Experimenter.Availability
import           Experimenter.ConcurrentIO
import           Experimenter.DatabaseSetting
import           Experimenter.DB
import           Experimenter.Eval.Reduce
import           Experimenter.Eval.Type       as E
import           Experimenter.Eval.Util
import           Experimenter.Experiment
import           Experimenter.Measure
import           Experimenter.Models
import           Experimenter.Result.Query
import           Experimenter.Result.Type
import           Experimenter.StepResult


genEvalsIO :: (ExperimentDef a, IO ~ ExpM a) => DatabaseSetting -> Experiments a -> [StatsDef a] -> IO (Evals a)
genEvalsIO = genEvals id


genEvals :: (ExperimentDef a) => (ExpM a (Evals a) -> IO (Evals a)) -> DatabaseSetting -> Experiments a -> [StatsDef a] -> IO (Evals a)
genEvals runExpM dbSetup exps evals = runExpM $ runDBWithM runResourceT dbSetup $ do
  res <- mapM (mkEvals evals) (exps ^. experiments)
  return $ Evals (exps {_experiments = []}) res


genEvalsConcurrent :: (ExperimentDef a) => Int -> (ExpM a (ExperimentEval a) -> IO (ExperimentEval a)) -> DatabaseSetting -> Experiments a -> [StatsDef a] -> IO (Evals a)
genEvalsConcurrent parallelWorkers runExpM dbSetup exps evals = do
  res <- mapConurrentIO parallelWorkers (runExpM . runDBWithM runResourceT dbSetup . mkEvals evals) (exps ^. experiments)
  return $ Evals (exps {_experiments = []}) res

mkEvals :: (ExperimentDef a ) => [StatsDef a] -> Experiment a -> DB (ExpM a) (ExperimentEval a)
mkEvals evals e = do
  liftIO $ putStrLn $ "Evaluating Experiment " ++ show (e ^. experimentNumber)
  xs <- mkTime "All Experiment Evaluations" $ mapMRnf evalStatsDef evals
  return $ force $ ExperimentEval (e ^. experimentNumber) xs (e {_experimentResults = []})
  where
    evalStatsDef statsDef = do
      -- liftIO $ putStrLn $ "Evaluating " ++ show statsDef
      Available <$> genExperiment e statsDef

mapMRnf :: (NFData b, Monad m) => (a -> m b) -> [a] -> m [b]
mapMRnf _ [] = return []
mapMRnf f (x:xs) = do
  !(force -> x') <- f x
  (x' :) <$!> mapMRnf f xs
-- mapMRnf :: (NFData b, Monad m) => (a -> m b) -> [a] -> m [b]
-- mapMRnf = mapM


genExperiment :: (ExperimentDef a) => Experiment a -> StatsDef a -> DB (ExpM a) (EvalResults a)
genExperiment exp (Named eval name) = addName <$!> genExperiment exp eval
  where addName res = res { _evalType = Named (res ^. evalType) name }
genExperiment exp (Name name eval) = addName <$!> genExperiment exp eval
  where addName res = res { _evalType = Named (res ^. evalType) name }
genExperiment exp eval =
  case eval of
    Mean OverExperimentRepetitions (Stats eval')   -> reduce (Stats eval') <$!> genExpRes id eval'
    Mean OverExperimentRepetitions eval'           -> reduce eval' <$!> genExpRes id (Id eval')
    Sum OverExperimentRepetitions (Stats eval')    -> reduce (Stats eval') <$!> genExpRes id eval'
    Sum OverExperimentRepetitions eval'            -> reduce eval' <$!> genExpRes id (Id eval')
    StdDev OverExperimentRepetitions (Stats eval') -> reduce (Stats eval') <$!> genExpRes id eval'
    StdDev OverExperimentRepetitions eval'         -> reduce eval' <$!> genExpRes id (Id eval')
    -- Mean (OverBestXExperimentRepetitions nr cmp) eval' -> reduce eval' <$> genExpRes (take nr . sortBy (cmp `on` id)) (Id eval')
    -- Sum (OverBestXExperimentRepetitions nr cmp) eval' -> reduce eval' <$> genExpRes (take nr . sortBy (cmp `on` id)) (Id eval')
    -- StdDev (OverBestXExperimentRepetitions nr cmp) eval' -> reduce eval' <$> genExpRes (take nr . sortBy (cmp `on` id)) (Id eval')
    _ -> packGenRes <$!> genExpRes id eval
  where
    packGenRes = EvalVector eval UnitExperimentRepetition
    genExpRes f e = mapMRnf (genExperimentResult exp e) (f $ exp ^. experimentResults)
    -- reduce = reduceUnary eval . EvalVector eval UnitExperimentRepetition
    reduce eval' = reduceUnary eval . EvalVector (Id eval') UnitExperimentRepetition


genExperimentResult :: (ExperimentDef a) => Experiment a -> StatsDef a -> ExperimentResult a -> DB (ExpM a) (EvalResults a)
genExperimentResult _ (Named _ n) _ = error $ "An evaluation may only be named on the outermost function in evaluation " <> T.unpack (E.decodeUtf8 n)
genExperimentResult _ (Name n _) _ = error $ "An evaluation may only be named on the outermost function in evaluation " <> T.unpack (E.decodeUtf8 n)
genExperimentResult exp eval expRes =
  case eval of
    Mean OverReplications (Stats eval')   -> reduce <$!> genRepl eval'
    Mean OverReplications eval'           -> reduce <$!> genRepl (Id eval')
    StdDev OverReplications (Stats eval') -> reduce <$!> genRepl eval'
    StdDev OverReplications eval'         -> reduce <$!> genRepl (Id eval')
    Sum OverReplications (Stats eval')    -> reduce <$!> genRepl eval'
    Sum OverReplications eval'            -> reduce <$!> genRepl (Id eval')
    _                                     -> packGenRes <$!> genRepl eval
  where
    packGenRes = EvalVector eval UnitReplications
    -- genRepl e = mapMRnf (fmap force . genReplication exp e) (expRes ^. evaluationResults)
    genRepl e = mapMRnf (genReplication exp e (expRes ^. repetitionNumber)) (expRes ^.. evaluationResults . traversed . filtered (\x -> isJust (x ^. evalResults)))
    reduce !inp = EvalVector eval (getUnit inp) $ map (reduceUnary eval) $ transpose UnitReplications inp
    getUnit (EvalVector _ unit _:_) = unit
    getUnit (EvalValue _ unit _ _ _:_) = unit
    getUnit (EvalReducedValue _ unit _:_) = unit
    getUnit [] = error "Unexpected empty data in getUnit in genExperimentResult in Eval.Ops"


genReplication :: (ExperimentDef a) => Experiment a -> StatsDef a -> Int -> ReplicationResult a -> DB (ExpM a) (EvalResults a)
genReplication exp eval repNr repl =
  mkTime ("\tExperiment " <> show (exp ^. experimentNumber) <> " Repetition " <> show repNr <> " Replication" <> show (repl ^. replicationNumber)) $
  fromMaybe (error "Evaluation data is incomplete!") <$!> sequence (genResultData exp eval <$!> (repl ^. evalResults))


-- stddevSamp     :: (E.PersistField a, E.PersistField b) => E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value (Maybe b))
-- stddevSamp     = unsafeSqlFunction "SUM"
--   where -- | (Internal) A raw SQL function.  Once again, the same warning
--        -- from 'unsafeSqlBinOp' applies to this function as well.
--        -- unsafeSqlFunction :: E.UnsafeSqlFunctionArgument a =>
--        --                      TLB.Builder -> a -> SqlExpr (Value b)
--        unsafeSqlFunction name arg =
--          ERaw Never $ \info ->
--            let (argsTLB, argsVals) =
--                  uncommas' $ map (\(E.ERaw _ f) -> f info) $ toArgList arg
--            in (name <> parens argsTLB, argsVals)

genResultData :: (ExperimentDef a) => Experiment a -> StatsDef a -> ResultData a -> DB (ExpM a) (EvalResults a)
genResultData _ (Named _ n) _ = error $ "An evaluation may only be named on the outermost function in evaluation " <> T.unpack (E.decodeUtf8 n)
genResultData _ (Name n _) _ = error $ "An evaluation may only be named on the outermost function in evaluation " <> T.unpack (E.decodeUtf8 n)
genResultData exp eval resData =
  case eval of
    Mean OverPeriods (Of name) -> aggregate name E.avg_
    Mean OverPeriods eval' -> reduceUnary eval <$!> genResultData exp (Id eval') resData
    -- StdDev OverPeriods (Of name) -> aggregate name E.stdDev_ -- not available in esqueleto :-o
    StdDev OverPeriods eval' -> reduceUnary eval <$!> genResultData exp (Id eval') resData
    Sum OverPeriods (Of name) -> aggregate name E.sum_
    Sum OverPeriods eval' -> reduceUnary eval <$!> genResultData exp (Id eval') resData
    Id eval' -> evalOf exp eval' resData
    _ -> genExperiment exp eval
  where
    aggregate name agg =
      fmap (EvalReducedValue eval UnitPeriods) $
      case resData ^. resultDataKey of
        ResultDataPrep k   -> loadPreparationAggregateWhere k agg (whereName name)
        ResultDataWarmUp k -> loadReplicationWarmUpAggregateWhere k agg (whereName name)
        ResultDataRep k    -> loadReparationAggregateWhere k agg (whereName name)
    whereName name =
      case resData ^. resultDataKey of
        ResultDataPrep{} -> PrepMeasureWhere $ \_ resStep -> E.where_ (resStep E.^. PrepResultStepName E.==. E.val (E.decodeUtf8 name))
        ResultDataWarmUp{} -> WarmUpMeasureWhere $ \_ resStep -> E.where_ (resStep E.^. WarmUpResultStepName E.==. E.val (E.decodeUtf8 name))
        ResultDataRep{} -> RepMeasureWhere $ \_ resStep -> E.where_ (resStep E.^. RepResultStepName E.==. E.val (E.decodeUtf8 name))


-- cacheMVar :: MVar (M.Map (Of a, ResultDataKey) (EvalResults a))
-- cacheMVar = unsafePerformIO $ newMVar mempty
-- {-# NOINLINE cacheMVar #-}

-- emptyCache :: IO ()
-- emptyCache = liftIO $ modifyMVar_ cacheMVar (const mempty)

-- addCache :: (Of a, ResultDataKey) -> EvalResults a -> IO ()
-- addCache k v = liftIO $ modifyMVar_ cacheMVar (return . M.insert k v)

-- lookupCache :: (Of a, ResultDataKey) -> IO (Maybe (EvalResults a))
-- lookupCache k = liftIO $ (M.lookup k =<<) <$> tryReadMVar cacheMVar


evalOf :: (ExperimentDef a) => Experiment a -> Of a -> ResultData a -> DB (ExpM a) (EvalResults a)
evalOf exp eval resData =
  case eval of
    Of name -> do
      -- mCache <- liftIO $ lookupCache (eval, resData ^. resultDataKey)
      -- case mCache of
        -- Nothing -> do
        --   res <- runConduit $ srcAvailableListWhere (whereName name) (resData ^. results) .| mapC (fromMeasure $ E.decodeUtf8 name) .| CL.consume
        --   let evalVector = EvalVector (Id $ Of name) UnitPeriods res
        --   liftIO $ addCache (eval, resData ^. resultDataKey) evalVector
        --   return evalVector
        -- Just evalVector -> return evalVector
      ----
      res <- runConduit $ srcAvailableListWhere (whereName name) (resData ^. results) .| mapC (fromMeasure $ E.decodeUtf8 name) .| CL.consume
      let evalVector = EvalVector (Id $ Of name) UnitPeriods res
      return evalVector

    Stats def -- alter experiment and restart evaluation
      | maybe False (> OverPeriods) (getOver def) -> do
        error "OverExperimentRepetitions and OverReplications have to be the two outermost evaluations or not present"
        -- let k = resData ^. resultDataKey
        --     isK = (== k)
        --     check Nothing = Nothing
        --     check (Just rD)
        --       | isK (rD ^. resultDataKey) = Just rD
        --       | otherwise = Nothing
        --     fil lns = over lns check
        -- let exp' = over (experimentResults . traversed) (over (evaluationResults . traversed) (fil warmUpResults . fil evalResults) . over preparationResults check) exp
        --     allResData x = x ^.. evaluationResults . traversed . evalResults . traversed ++ x ^.. evaluationResults . traversed . warmUpResults . traversed ++ x ^.. preparationResults . traversed
        --     expFiltered = over experimentResults (filter (not . null . allResData)) exp'
        -- liftIO $ putStrLn $ "Filtered: " ++ show (length $ concatMap allResData (expFiltered ^. experimentResults))
        -- genExperiment expFiltered def
    Stats def -> genResultData exp def resData
    Div eval1 eval2 -> reduceBinaryOf eval <$!> evalOf exp eval1 resData <*> evalOf exp eval2 resData
    Add eval1 eval2 -> reduceBinaryOf eval <$!> evalOf exp eval1 resData <*> evalOf exp eval2 resData
    Sub eval1 eval2 -> reduceBinaryOf eval <$!> evalOf exp eval1 resData <*> evalOf exp eval2 resData
    Mult eval1 eval2 -> reduceBinaryOf eval <$!> evalOf exp eval1 resData <*> evalOf exp eval2 resData
    First (Of name) -> do

      -- res <- aggregate (E.orderBy [E.asc (repM E.^. RepMeasurePeriod)]) name

      -- todo: sorting!!!
      res <- runConduit $ srcAvailableListWhere (whereName' (E.limit 1) name) (resData ^. results) .| mapC (fromMeasure $ E.decodeUtf8 name) .| headC
      return $ EvalVector (Id $ First $ Stats $ Id $ Of name) UnitPeriods [fromMaybe (error $ "empty elements in evalOf First(Of " <> show name <> ")") res]
    First eval' -> reduceUnaryOf eval <$!> evalOf exp eval' resData
    Last (Of name) -> do
      res <- runConduit $ srcAvailableListWhere (whereName name) (resData ^. results) .| mapC (fromMeasure $ E.decodeUtf8 name) .| lastC
      return $ EvalVector (Id $ Last $ Stats $ Id $ Of name) UnitPeriods [fromMaybe (error $ "empty elements in evalOf Last(Of " <> show name <> ")") res]
    Last eval' -> reduceUnaryOf eval <$!> evalOf exp eval' resData
    EveryXthElem _ eval' -> reduceUnaryOf eval <$!> evalOf exp eval' resData
    Length (Of name) -> do
      res <- runConduit $ srcAvailableListWhere (whereName name) (resData ^. results) .| mapC (fromMeasure $ E.decodeUtf8 name) .| lengthC
      return $ EvalReducedValue (Id $ Length $ Stats $ Id $ Of name) UnitScalar res
    Length eval' -> reduceUnaryOf eval <$!> evalOf exp eval' resData
  where
    whereName = whereName' (return ())
    whereName' add name =
      case resData ^. resultDataKey of
        ResultDataPrep _ -> PrepMeasureWhere $ \_ resStep -> E.where_ (resStep E.^. PrepResultStepName E.==. E.val (E.decodeUtf8 name)) >> add
        ResultDataWarmUp _ -> WarmUpMeasureWhere $ \_ resStep -> E.where_ (resStep E.^. WarmUpResultStepName E.==. E.val (E.decodeUtf8 name)) >> add
        ResultDataRep _ -> RepMeasureWhere $ \_ resStep -> E.where_ (resStep E.^. RepResultStepName E.==. E.val (E.decodeUtf8 name)) >> add
    -- aggregate add name agg =
    --     case resData ^. resultDataKey of
    --       ResultDataPrep k   -> loadPreparationAggregateWhere k agg (whereName' add name)
    --       ResultDataWarmUp k -> loadReplicationWarmUpAggregateWhere k agg (whereName' add name)
    --       ResultDataRep k    -> loadReparationAggregateWhere k agg (whereName' add name)

fromMeasure :: T.Text -> Measure -> EvalResults a
fromMeasure name (Measure p res) = case find ((==name) . view resultName) res of
  Nothing -> error $ "Variable with name " <> T.unpack name <> " could not be found!"
  Just (StepResult n mX y) -> EvalValue (Id $ Of $ E.encodeUtf8 n) UnitPeriods (E.encodeUtf8 n) (maybe (Left p) Right mX) y
