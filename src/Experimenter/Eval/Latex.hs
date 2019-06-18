{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
module Experimenter.Eval.Latex
    ( writeLatex
    , writeAndCompileLatex
    ) where

import           Control.Lens                 hiding ((&))
import           Control.Monad                (void, zipWithM_)
import           Control.Monad.Logger
import           Data.Either
import           Data.Function                (on)
import           Data.List                    as L (find, foldl', groupBy, nub, sortBy,
                                                    transpose)
-- import           Data.Matrix hiding (trace)
import           Data.Maybe                   (fromMaybe)
import qualified Data.Serialize               as S
import qualified Data.Text                    as T
import           System.Directory
import           System.Process
import           Text.LaTeX
import           Text.LaTeX.Base.Class
import           Text.LaTeX.Packages.AMSMath
import           Text.LaTeX.Packages.Inputenc

import           Experimenter.Eval.Table
import           Experimenter.Eval.Type
import           Experimenter.Experiment
import           Experimenter.Models
import           Experimenter.Parameter.Type
import           Experimenter.Result.Type
import           Experimenter.Util

import           Debug.Trace

instance (MonadLogger m) => MonadLogger (LaTeXT m) where


rootPath :: FilePath
rootPath = "results"

mainFile :: FilePath
mainFile = "main.tex"

mainFilePdf :: FilePath
mainFilePdf = T.unpack (T.dropWhileEnd (/= '.') (T.pack mainFile)) <> "pdf"

writeAndCompileLatex :: Evals a -> IO ()
writeAndCompileLatex evals = writeLatex evals >> compileLatex evals

getExpsName :: Evals a -> String
getExpsName evals  = T.unpack $ T.replace " " "_" $ evals ^. evalsExperiments.experimentsName

compileLatex :: Evals a -> IO ()
compileLatex evals = do
  let n = getExpsName evals
      dir = rootPath <> "/" <> n
  void $ runProcess "pdflatex" [mainFile] (Just dir) Nothing Nothing Nothing Nothing >>= waitForProcess
  pwd <- getCurrentDirectory
  putStrLn $ "\n\nSuccessfully compiled your results! See file://" <> pwd <> "/results/" <> n <> "/" <> mainFilePdf


writeLatex :: Evals a -> IO ()
writeLatex evals = do
  let n = getExpsName evals
      dir = rootPath <> "/" <> n
      file = dir <> "/" <> mainFile
  createDirectoryIfMissing True dir
  runStdoutLoggingT (execLaTeXT (root evals)) >>= renderFile file

root :: (MonadLogger m) => Evals a -> LaTeXT m ()
root evals = do
  thePreamble evals
  document $ theBody evals

-- Preamble with some basic info.
thePreamble :: (MonadLogger m) => Evals a -> LaTeXT m ()
thePreamble evals = do
  let n = evals ^. evalsExperiments.experimentsName
  documentclass [] article
  author "Manuel Schneckenreither"
  title $ "Evaluation for ``" <> raw n <> "''"
  usepackage [utf8] inputenc
  usepackage [] "fullpage"
  usepackage [] "array"
  usepackage [] amsmath

-- Body with a section.
theBody :: (MonadLogger m) => Evals a -> LaTeXT m ()
theBody evals = do
  maketitle
  experimentsInfo (evals ^. evalsExperiments)
  experimentsEvals evals

  -- mapM_ (experimentsEval evals) (evals ^. evalsResults)

  -- section "Hello"
 --  "This is a simple example using the "
 --  hatex
 --  " library. "
 -- -- 'textbf' turns characters to bold font (as you already may know).
 --  textbf "Enjoy!"
 --  " "
 -- -- This is how we nest commands.
 --  textbf (large "Yoohoo!")

-- refTblGenInfo :: LaTeXT IO ()
-- refTblGenInfo = "tbl:genInfo"

-- refTblParamSetting :: Int -> LaTeXT IO ()
-- refTblParamSetting nr = "tbl:paramSetting:" <> raw (tshow nr)

experimentsInfo :: (MonadLogger m) => Experiments a -> LaTeXT m ()
experimentsInfo exps = do
  part "General Information"
  printTable $ Table (Row ["Parameter", "Value"])
    [ Row ["Experiment Name: ",                           CellT (exps ^. experimentsName)]
    , Row ["Start time:",                                 CellT (tshow $ exps ^. experimentsStartTime)]
    , Row ["End time:",                                   CellT (maybe "" tshow (exps ^. experimentsEndTime))]
    , Row ["Number of conducted Experiments: ",           CellT (tshow $ length (exps ^. experiments))]
    , Row ["Experiment Repetitions (complete restarts):", CellT (tshow $ exps ^. experimentsSetup . expsSetupRepetitions)]
    , Row ["Experiment Preparation Steps:",               CellT (tshow $ exps ^. experimentsSetup . expsSetupPreparationSteps)]
    , Row ["Experiment Evaluation Warm Up Steps:",        CellT (tshow $ exps ^. experimentsSetup . expsSetupEvaluationWarmUpSteps)]
    , Row ["Experiment Evaluation Steps:",                CellT (tshow $ exps ^. experimentsSetup . expsSetupEvaluationSteps)]
    , Row ["Experiment Evaluation Replications:",         CellT (tshow $ exps ^. experimentsSetup . expsSetupEvaluationReplications)]
    ]

-- experimentsEval :: (MonadLogger m) => Evals a -> ExperimentEval a -> LaTeXT m ()
-- experimentsEval evals eval@(ExperimentEval nr res _) = do
--   pagebreak "4"
--   section $ "Experiment " <> raw (tshow nr)
--   -- paramSetting evals eval
--   -- experimentsResult evals
--   overReplicationResults evals

experimentsEvals :: (MonadLogger m) => Evals a -> LaTeXT m ()
experimentsEvals evals = do
  pagebreak "4"
  part "Experiment Evaluations"
  -- paramSetting evals eval
  -- experimentsResult evals
  overReplicationResults evals


overReplicationResults :: (MonadLogger m) => Evals a -> LaTeXT m ()
overReplicationResults evals = do
  let isOverReplication res = res ^. evalUnit == UnitReplications
      isOverExperiments res = res ^. evalUnit == UnitExperimentRepetition
      isOverPeriods res = res ^. evalUnit == UnitPeriods
      isEvalVectorOfPeriods (EvalVector _ _ vals) = all (== UnitPeriods) (vals ^.. traversed . evalUnit)
  let groupedEvals = map groupEvaluations (evals ^. evalsResults)
  -- let evalsExps = over (evalsResults . traversed . evalExperimentResults) (sortBy (compare `on` view evalType) . filter isOverExperiments) evals
  -- let evalsExpsPer = over (evalsResults . traversed . evalExperimentResults) (filter isEvalVectorOfPeriods) evalsExps
  -- let evalsRepls = over (evalsResults . traversed . evalExperimentResults) (sortBy (compare `on` view evalType) . filter isOverReplication) evals
  -- let evalsPeri = over (evalsResults . traversed . evalExperimentResults) (sortBy (compare `on` view evalType) . filter isOverPeriods) evals
  -- let tblsExps = map (mkExperimentTableExp evalsExps) (evalsExps ^. evalsResults)
  -- let tblsExpsPer = map (mkExperimentTableExp evalsExps) (evalsExpsPer ^. evalsResults)
  -- let tblsRepls = map (mkExperimentTableExp evalsRepls) (evalsRepls ^. evalsResults)
  let isPeriodicEval (UnitPeriods, _, _) = True
      isPeriodicEval _                   = False
  let isReplicationEval (UnitReplications, _, _) = True
      isReplicationEval _                        = False
  let isExperimentalReplicationEval (UnitExperimentRepetition, _, _)         = True
      isExperimentalReplicationEval (UnitBestExperimentRepetitions {}, _, _) = True
      isExperimentalReplicationEval _                                        = False
  let periodicTbls = map (map (mkExperimentTable evals)) $ transpose $ filter (isPeriodicEval . head) $ transpose groupedEvals
  let replicationTbls = map (map (mkExperimentTable evals)) $ transpose $ filter (isReplicationEval . head) $ transpose groupedEvals
  let experimentalReplicationTbls = map (map (mkExperimentTable evals)) $ transpose $ filter (isExperimentalReplicationEval . head) $ transpose groupedEvals
  section $ "Periodic Evaluations"
  zipWithM_
    (\nr exps ->
       mapM_
         (\(mPs, vs) -> do
            subsection $ "Experiment No. " <> raw (tshow nr)
            maybe "There are no configured parameters!" printTable mPs
            mapM_ printTableWithName vs)
         exps)
    [1 ..]
    periodicTbls
  section $ "Replication Evaluations"
  zipWithM_
    (\nr exps ->
       (mapM_
          (\(mPs, vs) -> do
             subsection $ "Experiment No. " <> raw (tshow nr)
             maybe "There are no configured parameters!" printTable mPs
             mapM_ printTableWithName vs)
          exps))
    [1 ..]
    replicationTbls
  section $ "Repetition Evaluations"
  zipWithM_
    (\nr exps ->
       (mapM_
          (\(mPs, vs) -> do
             subsection $ "Experiment No. " <> raw (tshow nr)
             maybe "There are no configured parameters!" printTable mPs
             mapM_ printTableWithName vs)
          exps))
    [1 ..]
    experimentalReplicationTbls

printTableWithName :: (Monad m, MonadLogger m) => (StatsDef a, Table) -> LaTeXT m ()
printTableWithName (nm, tbl) = do
  paragraph (raw $ prettyStatsDef nm)
  printTable tbl

groupEvaluations :: ExperimentEval a -> [(Unit, ExperimentEval a, [EvalResults a])]
groupEvaluations eval@(ExperimentEval _ res _) = map (\xs@(x:_) -> (leastUnit x, eval, xs)) $ groupBy ((==) `on` minMaxUnits) $ sortBy (compare `on` minMaxUnits) res
  where
    minMaxUnits e = (e ^. evalUnit, leastUnit e)


leastUnit :: EvalResults a -> Unit
leastUnit (EvalValue _ u _ _ _)    = u
leastUnit (EvalReducedValue _ u _) = u
leastUnit (EvalVector _ _ vals)    = leastUnit (head vals)


unpackUntil :: Unit -> EvalResults a -> [EvalResults a]
unpackUntil unit res | res ^. evalUnit == unit = [res]
                     | otherwise = case res of
                         EvalVector{} -> concatMap (unpackUntil unit) (res ^. evalValues)
                         _            -> error $ "cannot unpack res: " <> show res

mkNamesUntil :: Unit -> EvalResults a -> [Text]
mkNamesUntil unit res
  | res ^. evalUnit == unit = [mempty]
  | otherwise =
    case res of
      EvalVector _ u vals ->
        case u of
          UnitPeriods -> [mempty]
          UnitReplications -> map (\x -> "Rpl " <> tshow x <> ": ") [1 .. length vals]
          _ ->
            let sub = head $ map (mkNamesUntil unit) vals
            in concatMap (\x -> map (\s -> "Rpt " <> tshow x <> ": " <> s) sub) [1 .. length vals]
      _ -> error $ "cannot unpack res: " <> show res


mkExperimentTable :: Evals a -> (Unit, ExperimentEval a, [EvalResults a]) -> (Maybe Table, [(StatsDef a, Table)])
mkExperimentTable evals (lowestUnit, eval, res) =
  let params = paramSettingTable evals eval
      resUnit = map ((names, ) . unpackUntil lowestUnit) res
      tableRes = map (uncurry (zipWith mkEvalResult)) resUnit
      tbls = map toTables tableRes
      names = (map (map CellT . return) . mkNamesUntil lowestUnit) (head res)
      evalStatDefs = map (^. evalType) res
   in (params, zip evalStatDefs tbls)

-- mkExperimentTable evals (UnitBestExperimentRepetitions _, eval, res) = mkExperimentTable evals (UnitExperimentRepetition, eval, res)
-- mkExperimentTable evals (UnitExperimentRepetition, eval, res) = undefined


data TableResult = TableResult
  { reducedOver :: Maybe Unit
  , header      :: Row
  , rows        :: [Row]
  } deriving (Show)

toTable :: TableResult -> Table
toTable (TableResult _ h rs) = Table h rs

toTables :: [TableResult] -> Table
toTables xs = Table (header $ head xs) (concatMap rows xs)


mkEvalResult :: [Cell] -> EvalResults a -> TableResult
mkEvalResult _ (EvalVector _ unit []) = error "Empty evaluation. Check your eval setup."
mkEvalResult name (EvalVector _ unit vals) =
  TableResult (demoteUnit unit) (Row $ CellT (unitName unit) : map (CellT . tshow) [1 .. length vals])
  (map Row (foldl' mkRows [name] rowVals))
  where
    subVals = map (mkEvalResult []) vals
    rowVals = map rows subVals
    mkRows :: [[Cell]] -> [Row] -> [[Cell]]
    mkRows accs vs = zipWith (++) accs (map fromRow vs)
    fromRow (Row xs) = xs
    unitName UnitPeriods = "Period:"
    unitName UnitReplications = "Replication:"
    unitName UnitExperimentRepetition = "Experiment Repetition:"
    unitName (UnitBestExperimentRepetitions bestNr) = "Best " <> tshow bestNr <> " Experiment Repetitions:"
mkEvalResult [] (EvalValue _ u n x y) = TableResult Nothing (Row [getXValue x]) [Row [CellD y]]
  where getXValue (Left x)  = CellT $ tshow x
        getXValue (Right d) = CellD d
mkEvalResult names@(n1:_) (EvalValue _ u n x y) = TableResult Nothing (Row [CellEmpty, getXValue x]) [Row [n1, CellD y]]
  where getXValue (Left x)  = CellT $ tshow x
        getXValue (Right d) = CellD d
mkEvalResult [] (EvalReducedValue statsDef u y) = TableResult (Just u) (Row [CellT $ tshow statsDef]) [Row [CellD y]]
mkEvalResult names@(n:_) (EvalReducedValue statsDef u y) = TableResult (Just u) (Row [CellEmpty , CellT $ prettyStatsDef statsDef]) [Row [n, CellD y]]


-- experimentsResult :: (MonadLogger m) => Evals a -> LaTeXT m ()
-- experimentsResult evals = do
--   subsection $ "Evaluation over Experiments"
--   let exps = evals ^. evalsExperiments
--       paramSetups = view experimentsParameters exps
--   mapM_ (experimentResultForParam evals) paramSetups


-- experimentResultForParam :: (MonadLogger m) => Evals a -> ParameterSetup a -> LaTeXT m ()
-- experimentResultForParam evals (ParameterSetup paramName setter getter _ (minV, maxV)) = do
--   -- let paramValuesBS = L.nub $ eval ^.. evalExperiment . parameterSetup . traversed . filtered ((== paramName) . view parameterSettingName) . parameterSettingValue
--   --     fromRight (Right x) = x
--   --     fromRight _ = error $ "Could not deserialise parameter values of parameter " <> T.unpack paramName
--   --     eiParamValues = map (S.runGet S.get) paramValuesBS
--   --     otherParams = eval ^.. evalExperiment . parameterSetup . traversed . filtered ((/= paramName) . view parameterSettingName)
--   --     unused = setter (fromRight $ head eiParamValues) (evals ^. evalsExperiments . experimentsInitialState)

--   -- case find isLeft eiParamValues of
--   --   Just (Left err) -> $(logDebug) $ "Could not deserialise parameter values of parameter " <> paramName <> ". Skipping this evaluation. Error was: " <> T.pack err
--   --   Nothing -> do
--   --     let paramValues = map fromRight eiParamValues
--   --     -- let otherParamGrouped =

--   --     -- let cmp (ExperimentEval )
--   --     -- let ress = L.groupBy ((==) `on` cmp) $ L.sortBy (compare `on` cmp) res
--   --     -- undefined

--       return ()


paramSetting :: (MonadLogger m) => Evals a -> ExperimentEval a -> LaTeXT m ()
paramSetting evals expEval@(ExperimentEval nr _ exp) = do
  subsection $ "Parameter Setting of Experiment No. " <> raw (tshow nr)
  maybe "There are no configured parameters!" printTable (paramSettingTable evals expEval)

paramSettingTable :: Evals a -> ExperimentEval a -> Maybe Table
paramSettingTable evals (ExperimentEval nr _ exp)
  | null (exp ^. parameterSetup) = Nothing
  | otherwise = Just $ Table (Row ["Parameter", "Value"]) (map mkRow (exp ^. parameterSetup))
  where
    mkRow :: ParameterSetting a -> Row
    mkRow (ParameterSetting n bsV) =
      case find ((== n) . parameterName) (evals ^. evalsExperiments . experimentsParameters) of
        Nothing -> Row [CellT n, "was not modified as it is not listed in the parameter setting"]
        Just (ParameterSetup _ setter _ _ mBounds) ->
          case S.runGet S.get bsV of
            Left err -> Row [CellT n, CellT (T.pack err)]
            Right val ->
              let _ = setter val (evals ^. evalsExperiments . experimentsInitialState) -- only needed for type inference
              in Row
                   [ CellT n
                   , CellL $ raw (tshow val) <>
                     case mBounds of
                       Nothing -> ""
                       Just (minVal, maxVal) -> math (text " " `in_` autoParens (text (raw (tshow minVal)) <> ", " <> text (raw (tshow maxVal))))
                   ]
