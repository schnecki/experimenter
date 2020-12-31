{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Experimenter.Eval.Latex
    ( writeLatex
    , writeAndCompileLatex
    ) where

import           Control.Arrow                ((&&&))
import           Control.DeepSeq
import           Control.Lens                 hiding ((&))
import           Control.Monad                (forM, unless, void)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.List                    as L (find, foldl')
import qualified Data.Map.Strict              as M
import qualified Data.Serialize               as S
import qualified Data.Text                    as T
import           GHC.Generics
import           System.Directory
import           System.FilePath.Posix
import           System.Posix.User
import           System.Process
import           Text.LaTeX
import           Text.LaTeX.Packages.AMSMath
import           Text.LaTeX.Packages.Hyperref
import           Text.LaTeX.Packages.Inputenc
import           Text.LaTeX.Packages.TabularX

import           Experimenter.Availability
import           Experimenter.DatabaseSetting
import           Experimenter.DB
import           Experimenter.Eval.Table
import           Experimenter.Eval.Type
import           Experimenter.Eval.Util
import           Experimenter.Models
import           Experimenter.Parameter.Type
import           Experimenter.Result.Type
import           Experimenter.Setting         (ExperimentInfoParameter (..))
import           Experimenter.Util


import           Debug.Trace

writeAndCompileLatex :: DatabaseSetting -> Evals a -> IO ()
writeAndCompileLatex dbSetup evals = writeLatex dbSetup evals >> compileLatex evals

compileLatex :: Evals a -> IO ()
compileLatex evals = do
  let exps = evals ^. evalsExperiments
      dir = expsPath exps
      n = getExpsName exps
  void $ runProcess "pdflatex" [mainFile exps] (Just dir) Nothing Nothing Nothing Nothing >>= waitForProcess
  pwd <- getCurrentDirectory
  putStrLn $ "n\nSuccessfully compiled your results! See file://" <> pwd <> "/results/" <> n <> "/" <> mainFilePdf exps


writeLatex :: DatabaseSetting -> Evals a -> IO ()
writeLatex dbSetup evals = do
  let exps = evals ^. evalsExperiments
      dir = expsPath exps
      file = dir </> mainFile exps
  liftIO $ createDirectoryIfMissing True dir
  res <- runDBSimple dbSetup $ execLaTeXT (root evals)
  renderFile file res


root :: Evals a -> LaTeXT SimpleDB ()
root evals = do
  thePreamble evals
  document $ theBody evals


-- Preamble with some basic info.
thePreamble :: (MonadIO m) => Evals a -> LaTeXT m ()
thePreamble evals = do
  let n = evals ^. evalsExperiments . experimentsName
  documentclass [] article
  user <- liftIO getLoginName
  author ("Username: " <> fromString user)
  title $ "Evaluation for ``" <> raw n <> "''"
  usepackage [utf8] inputenc
  usepackage [] "fullpage"
  usepackage [] "array"
  usepackage [] amsmath
  usepackage [pdftex] hyperref
  usepackage [] tabularxp

-- Body with a section.
theBody :: Evals a -> LaTeXT SimpleDB ()
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

  let infoParams = trace( "Info Params: " ++ show (length $ exps ^. experimentsInfoParameters)) $ exps ^. experimentsInfoParameters


  part "Experiment Information Parameters"
  if null infoParams
    then center $ text "No Information Parameters were defined in the experiment setup"
    else printTextwidthTable $ Table (Row ["Parameter", "Value"]) (map mkInfoParam infoParams)

  where mkInfoParam (ExperimentInfoParameter n v) = Row [CellT n, CellT (tshow v)]


experimentsEvals :: Evals a -> LaTeXT SimpleDB ()
experimentsEvals evals = do
  pagebreak (pure "4")
  part "Experiment Evaluations"
  !(force -> tablesP) <- lift $! mkResultTablesFor UnitPeriods evals
  !(force -> tablesR) <- lift $! mkResultTablesFor UnitReplications evals
  !(force -> tablesE) <- lift $! mkResultTablesFor UnitExperimentRepetition evals
  !(force -> tablesS) <- lift $! mkResultTablesFor UnitScalar evals
  let tables = tablesP <> tablesR <> tablesE <> tablesS
      mParams = M.fromList $ map (view evalExperimentNumber &&& paramSettingTable evals) (evals ^. evalsResults)
  writeTables mParams (force tables)


data EvalTables a = EvalTables
  { periodic               :: ![(Int, [(StatsDef a, Table)])]
  , replications           :: ![(Int, [(StatsDef a, Table)])]
  , experimentReplications :: ![(Int, [(StatsDef a, Table)])]
  , numbers                :: ![(Int, [(StatsDef a, Table)])]
  } deriving (Show, Generic, NFData)

instance Semigroup (EvalTables a) where
  EvalTables ap ar ae an <> EvalTables bp br be bn = EvalTables (ap <> bp) (ar <> br) (ae <> be) (an <> bn)

instance Monoid (EvalTables a) where
  mempty = EvalTables [] [] [] []


mkResultTablesFor :: Unit -> Evals a -> SimpleDB [EvalTables a]
mkResultTablesFor unit evals =
  fmap force $!
  forM (evals ^. evalsResults) $ \eval@(ExperimentEval _ avRes _) ->
    fmap (force . mconcat) $!
    forM avRes $ \av -> do
      res <- mkTransientlyAvailable av
      return $
        let ~tbl = mkExperimentTable evals (leastUnit res, eval, [res])
         in case leastUnit res of
              UnitPeriods              -> EvalTables [tbl | unit == UnitPeriods] [] [] []
              UnitReplications         -> EvalTables [] [tbl | unit == UnitReplications] [] []
              UnitExperimentRepetition -> EvalTables [] [] [tbl | unit == UnitExperimentRepetition] []
              UnitScalar               -> EvalTables [] [] [] [tbl | unit == UnitScalar]


writeTables :: M.Map Int (Maybe Table) -> [EvalTables a] -> LaTeXT SimpleDB ()
writeTables params !(force -> tables) = do
  void $ writeTableFor "Scalar Value" (concatMap numbers tables)
  void $ writeTableFor "Repetition" (concatMap experimentReplications tables)
  void $ writeTableFor "Replications" (concatMap replications tables)
  void $ writeTableFor "Periodic" (concatMap periodic tables)
  where
    writeTableFor :: LaTeXT SimpleDB () -> [(Int, [(StatsDef a, Table)])] -> LaTeXT SimpleDB [()]
    writeTableFor name tbls = do
      section (name <> " Evaluations")
      forM (M.keys params) $ \k -> do
        let tblsFiltered = filter  (( == k) . fst) tbls
        unless (null tblsFiltered) $ do
          subsection $ "Experiment No. " <> raw (tshow k)
          maybe "There are no configured parameters!" printTextwidthTable (M.findWithDefault Nothing k params)
          mapM_ (\(_, tbls') -> forM tbls' $ \statsDefTbl -> printTableWithName statsDefTbl) tblsFiltered


printTableWithName :: (MonadLogger m) => (StatsDef a, Table) -> LaTeXT m ()
printTableWithName (nm, tbl) = do
  paragraph (raw $ prettyStatsDef nm)
  printTable tbl


leastUnit :: EvalResults a -> Unit
leastUnit (EvalValue _ u _ _ _)    = u
leastUnit (EvalReducedValue _ u _) = u
leastUnit (EvalVector _ u [])      = u
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


mkExperimentTable :: Evals a -> (Unit, ExperimentEval a, [EvalResults a]) -> (Int, [(StatsDef a, Table)])
mkExperimentTable _ (lowestUnit, eval, res) =
  let resUnit = map (\rs -> (mkNames rs, unpackUntil lowestUnit rs)) res
      tableRes = map (uncurry (zipWith (mkEvalResult lowestUnit))) resUnit
      tbls = map toTables tableRes
      mkNames = map (map CellT . return) . mkNamesUntil lowestUnit
      evalStatDefs = map (^. evalType) res
   in (eval ^. evalExperimentNumber, zip evalStatDefs tbls)


data TableResult = TableResult
  { header :: !Row
  , rows   :: ![Row]
  } deriving (Show)

toTables :: [TableResult] -> Table
toTables xs = Table (header $ head xs) (concatMap rows xs)

mkEvalResult :: Unit -> [Cell] -> EvalResults a -> TableResult
mkEvalResult _ _ (EvalVector _ _ []) = error "Empty evaluation. Check your eval setup."
mkEvalResult leastUnit' name (EvalVector _ _ vals) =
  TableResult (Row $ CellT (unitName leastUnit') : map (CellT . tshow) [1 .. length vals]) (map Row (foldl' mkRows [name] rowVals))
  where
    subVals = map (mkEvalResult leastUnit' []) vals
    rowVals = map rows subVals
    mkRows :: [[Cell]] -> [Row] -> [[Cell]]
    mkRows accs vs = zipWith (++) accs (map fromRow vs)
    fromRow (Row xs) = xs
    unitName UnitPeriods              = "Period:"
    unitName UnitReplications         = "Replication:"
    unitName UnitExperimentRepetition = "Experiment Repetition:"
    -- unitName (UnitBestExperimentRepetitions bestNr) = "Best " <> tshow bestNr <> " Experiment Repetitions:"
    unitName UnitScalar               = "Value:"
mkEvalResult _ [] (EvalValue _ _ _ x y) = TableResult (Row [getXValue x]) [Row [CellD y]]
  where getXValue (Left v)  = CellT $ tshow v
        getXValue (Right d) = CellD d
mkEvalResult _ (n1:_) (EvalValue _ _ _ x y) = TableResult (Row [CellEmpty, getXValue x]) [Row [n1, CellD y]]
  where getXValue (Left v)  = CellT $ tshow v
        getXValue (Right d) = CellD d
mkEvalResult _ [] (EvalReducedValue statsDef _ y) = TableResult (Row [CellT $ tshow statsDef]) [Row [CellD y]]
mkEvalResult _ (n:_) (EvalReducedValue statsDef _ y) = TableResult (Row [CellEmpty , CellT $ prettyStatsDef statsDef]) [Row [n, CellD y]]


paramSettingTable :: Evals a -> ExperimentEval a -> Maybe Table
paramSettingTable evals (ExperimentEval _ _ exp)
  | null (exp ^. parameterSetup) = Nothing
  | otherwise = Just $ Table (Row ["Parameter", "Value"]) (concatMap mkRow (exp ^. parameterSetup))
  where
    dropRow :: Row
    dropRow = Row [CellT "Skip Preparation Phase", CellT "True (No preparation phase was executed!)"]
    singleInstanceRow :: Text -> Row
    singleInstanceRow n = Row [CellT "Run a single instance", CellT $ "True (No further variations needed as specified by parameter " <> n <> "!)"]
    mkRow :: ParameterSetting a -> [Row]
    mkRow (ParameterSetting n bsV drp design) =
      case find ((== n) . parameterName) (evals ^. evalsExperiments . experimentsParameters) of
        Nothing ->
          Row
            [ CellT n
            , CellT $ "was not modified as it is not listed in the parameter setting" <>
              (if drp
                 then " [SkipPrepPhase]"
                 else "") <>
              (case design of
                 FullFactory    -> ""
                 SingleInstance -> "[SingleInstance]")
            ] :
          [dropRow | drp] ++
          [singleInstanceRow n | drp]
        Just (ParameterSetup _ setter _ _ mBounds _ _) ->
          case S.runGet S.get bsV of
            Left err -> [Row [CellT n, CellT (T.pack err)]]
            Right val ->
              let _ = setter val (evals ^. evalsExperiments . experimentsInitialState) -- only needed for type inference
               in Row
                    [ CellT n
                    , CellL $ raw (dereferLatex $ tshow val) <>
                      (case mBounds of
                         Nothing -> ""
                         Just (minVal, maxVal) -> math (text " " `in_` autoParens (text (raw (dereferLatex $ tshow minVal)) <> ", " <> text (raw (dereferLatex $ tshow maxVal))))) <>
                      (if drp
                         then " [SkipPrepPhase]"
                         else mempty) <>
                      (case design of
                         FullFactory    -> ""
                         SingleInstance -> "[SingleInstance]")
                    ] :
                  [dropRow | drp] ++
                  [singleInstanceRow n | drp]
