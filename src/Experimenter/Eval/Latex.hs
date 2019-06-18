{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Experimenter.Eval.Latex
    ( writeLatex
    , writeAndCompileLatex
    ) where

import           Control.Lens                 hiding ((&))
import           Control.Monad                (unless, void, zipWithM_)
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


experimentsEvals :: (MonadLogger m) => Evals a -> LaTeXT m ()
experimentsEvals evals = do
  pagebreak "4"
  part "Experiment Evaluations"
  overReplicationResults evals


overReplicationResults :: (MonadLogger m) => Evals a -> LaTeXT m ()
overReplicationResults evals = do
  -- let isOverReplication res = res ^. evalUnit == UnitReplications
  --     isOverExperiments res = res ^. evalUnit == UnitExperimentRepetition
  --     isOverPeriods res = res ^. evalUnit == UnitPeriods
  --     isEvalVectorOfPeriods (EvalVector _ _ vals) = all (== UnitPeriods) (vals ^.. traversed . evalUnit)
  -- let isPeriodicEval (UnitPeriods, _, _) = True
  --     isPeriodicEval _                   = False
  -- let isReplicationEval (UnitReplications, _, _) = True
  --     isReplicationEval _                        = False
  let isExperimentalReplicationUnit UnitExperimentRepetition        = True
      isExperimentalReplicationUnit UnitBestExperimentRepetitions{} =True
      isExperimentalReplicationUnit _                               = False
  let groupedEvals = map groupEvaluations (evals ^. evalsResults)
  let periodEvals = filter ((== UnitPeriods) . (^._1) . head) groupedEvals
      replicEvals = filter ((== UnitReplications) . (^._1) . head) groupedEvals
      expereEvals = filter (isExperimentalReplicationUnit . (^._1) . head) groupedEvals

  let periodicTbls = map (map (mkExperimentTable evals)) periodEvals
  let replicationTbls = map (map (mkExperimentTable evals)) replicEvals
  let experimentalReplicationTbls = map (map (mkExperimentTable evals)) expereEvals
  unless (null periodicTbls) $ do
    section "Periodic Evaluations"
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
  unless (null replicationTbls) $ do
    section "Replication Evaluations"
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
  unless (null experimentalReplicationTbls) $ do
    section "Repetition Evaluations"
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
leastUnit (EvalVector _ u vals)    = min u (leastUnit (head vals))


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
            in concatMap (\x -> map (\s -> if T.isInfixOf "Rpt" s then s else "Rpt " <> tshow x <> ": " <> s) sub) [1 .. length vals]
      _ -> error $ "cannot unpack res: " <> show res


mkExperimentTable :: Evals a -> (Unit, ExperimentEval a, [EvalResults a]) -> (Maybe Table, [(StatsDef a, Table)])
mkExperimentTable evals (lowestUnit, eval, res) =
  let params = paramSettingTable evals eval
      resUnit = map (\rs -> (mkNames rs,unpackUntil lowestUnit rs)) res
      tableRes = map (uncurry (zipWith (mkEvalResult lowestUnit))) resUnit
      tbls = map toTables tableRes
      mkNames = map (map CellT . return) . mkNamesUntil lowestUnit
      evalStatDefs = map (^. evalType) res
   in (params, zip evalStatDefs tbls)


data TableResult = TableResult
  { header :: Row
  , rows   :: [Row]
  } deriving (Show)

toTables :: [TableResult] -> Table
toTables xs = Table (header $ head xs) (concatMap rows xs)

mkEvalResult :: Unit -> [Cell] -> EvalResults a -> TableResult
mkEvalResult leastUnit _ (EvalVector _ unit []) = error "Empty evaluation. Check your eval setup."
mkEvalResult leastUnit name eval@(EvalVector _ unit vals) =
  TableResult (Row $ CellT (unitName leastUnit) : map (CellT . tshow) [1 .. length vals]) (map Row (foldl' mkRows [name] rowVals))
  where
    subVals = map (mkEvalResult leastUnit []) vals
    rowVals = map rows subVals
    mkRows :: [[Cell]] -> [Row] -> [[Cell]]
    mkRows accs vs = zipWith (++) accs (map fromRow vs)
    fromRow (Row xs) = xs
    unitName UnitPeriods = "Period:"
    unitName UnitReplications = "Replication:"
    unitName UnitExperimentRepetition = "Experiment Repetition:"
    unitName (UnitBestExperimentRepetitions bestNr) = "Best " <> tshow bestNr <> " Experiment Repetitions:"
mkEvalResult leastUnit [] (EvalValue _ u n x y) = TableResult (Row [getXValue x]) [Row [CellD y]]
  where getXValue (Left x)  = CellT $ tshow x
        getXValue (Right d) = CellD d
mkEvalResult leastUnit names@(n1:_) (EvalValue _ u n x y) = TableResult (Row [CellEmpty, getXValue x]) [Row [n1, CellD y]]
  where getXValue (Left x)  = CellT $ tshow x
        getXValue (Right d) = CellD d
mkEvalResult leastUnit [] (EvalReducedValue statsDef u y) = TableResult (Row [CellT $ tshow statsDef]) [Row [CellD y]]
mkEvalResult leastUnit names@(n:_) (EvalReducedValue statsDef u y) = TableResult (Row [CellEmpty , CellT $ prettyStatsDef statsDef]) [Row [n, CellD y]]


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
