{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Experimenter.Eval.Latex
    ( writeLatex
    , writeAndCompileLatex
    ) where

import           Control.Lens                 hiding ((&))
import           Control.Monad                (void)
import           Control.Monad.Logger
import           Data.Either
import           Data.Function                (on)
import           Data.List                    as L (find, groupBy, nub, sortBy)
import           Data.Matrix
import           Data.Maybe                   (fromMaybe)
import qualified Data.Serialize               as S
import qualified Data.Text                    as T
import           System.Directory
import           System.Process
import           Text.LaTeX
import           Text.LaTeX.Base.Class
import           Text.LaTeX.Packages.AMSMath
import           Text.LaTeX.Packages.Inputenc

import           Experimenter.Eval.Type
import           Experimenter.Experiment
import           Experimenter.Models
import           Experimenter.Parameter.Type
import           Experimenter.Result.Type
import           Experimenter.Util

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
  part "Evaluations"
  mapM_ (experimentsEval evals) (evals ^. evalsResults)

  -- section "Hello"
 --  "This is a simple example using the "
 --  hatex
 --  " library. "
 -- -- 'textbf' turns characters to bold font (as you already may know).
 --  textbf "Enjoy!"
 --  " "
 -- -- This is how we nest commands.
 --  textbf (large "Yoohoo!")

refTblGenInfo :: LaTeXT IO ()
refTblGenInfo = "tbl:genInfo"

refTblParamSetting :: Int -> LaTeXT IO ()
refTblParamSetting nr = "tbl:paramSetting:" <> raw (tshow nr)

experimentsInfo :: (MonadLogger m) => Experiments a -> LaTeXT m ()
experimentsInfo exps = do
  part "General Information"
  -- table (Just Bottom) $
  center $ do
    tabular Nothing [VerticalLine, LeftColumn, LeftColumn, VerticalLine] $ do
      hline
      textbf "Parameter" & textbf "Value" <> lnbk
      hline <> line "Experiment Name: " (exps ^. experimentsName) <> line "Start time:" (tshow $ exps ^. experimentsStartTime)
      line "End time:" (maybe "" tshow (exps ^. experimentsEndTime))
      line "Number of conducted Experiments: " (tshow $ length (exps ^. experiments))
      line "Experiment Repetitions (complete restarts):" (tshow $ exps ^. experimentsSetup . expsSetupRepetitions)
      line "Experiment Preparation Steps:" (tshow $ exps ^. experimentsSetup . expsSetupPreparationSteps)
      line "Experiment Evaluation Warm Up Steps:" (tshow $ exps ^. experimentsSetup . expsSetupEvaluationWarmUpSteps)
      line "Experiment Evaluation Steps:" (tshow $ exps ^. experimentsSetup . expsSetupEvaluationSteps)
      line "Experiment Evaluation Replications:" (tshow $ exps ^. experimentsSetup . expsSetupEvaluationReplications)
      hline
    -- label refTblGenInfo
    -- caption "General information for all conducted experiments."
  where
    line name value = raw name & raw value <> lnbk

experimentsEval :: (MonadLogger m) => Evals a -> ExperimentEval a -> LaTeXT m ()
experimentsEval evals eval@(ExperimentEval nr res _) = do
  pagebreak "4"
  section $ "Experiment " <> raw (tshow nr)
  paramSetting evals eval
  -- experimentsResult evals
  overReplicationResults evals


overReplicationResults :: (MonadLogger m) => Evals a -> LaTeXT m ()
overReplicationResults evals = do
  let isOverReplication (EvalVector _ UnitReplications _) = True
      isOverReplication _                                 = False
  let evals' = over (evalsResults.traversed.evalExperimentResults) (sortBy (compare `on` view evalType) . filter isOverReplication) evals
  mapM_ mkExperimentTable (evals' ^. evalsResults)


mkExperimentTable :: (MonadLogger m) => ExperimentEval a -> LaTeXT m ()
mkExperimentTable (ExperimentEval nr res exp) = do

  return ()


experimentsResult :: (MonadLogger m) => Evals a -> LaTeXT m ()
experimentsResult evals = do
  subsection $ "Evaluation over Experiments" -- " No. " <> raw (tshow nr)
  let exps = evals ^. evalsExperiments
      paramSetups = view experimentsParameters exps
  mapM_ (experimentResultForParam evals) paramSetups


experimentResultForParam :: (MonadLogger m) => Evals a -> ParameterSetup a -> LaTeXT m ()
experimentResultForParam evals (ParameterSetup paramName setter getter _ (minV, maxV)) = do
  -- let paramValuesBS = L.nub $ eval ^.. evalExperiment . parameterSetup . traversed . filtered ((== paramName) . view parameterSettingName) . parameterSettingValue
  --     fromRight (Right x) = x
  --     fromRight _ = error $ "Could not deserialise parameter values of parameter " <> T.unpack paramName
  --     eiParamValues = map (S.runGet S.get) paramValuesBS
  --     otherParams = eval ^.. evalExperiment . parameterSetup . traversed . filtered ((/= paramName) . view parameterSettingName)
  --     unused = setter (fromRight $ head eiParamValues) (evals ^. evalsExperiments . experimentsInitialState)

  -- case find isLeft eiParamValues of
  --   Just (Left err) -> $(logDebug) $ "Could not deserialise parameter values of parameter " <> paramName <> ". Skipping this evaluation. Error was: " <> T.pack err
  --   Nothing -> do
  --     let paramValues = map fromRight eiParamValues
  --     -- let otherParamGrouped =

  --     -- let cmp (ExperimentEval )
  --     -- let ress = L.groupBy ((==) `on` cmp) $ L.sortBy (compare `on` cmp) res
  --     -- undefined

      return ()


paramSetting :: (MonadLogger m) => Evals a -> ExperimentEval a -> LaTeXT m ()
paramSetting evals (ExperimentEval nr _ exp) = do
  subsection $ "Parameter Setting of Experiment No. " <> raw (tshow nr)
  if null (exp ^. parameterSetup)
    then "There are no configured parameters!"
    else center $ tabular Nothing [VerticalLine ,LeftColumn, LeftColumn, VerticalLine] $ do
           hline
           textbf "Parameter" & textbf "Value" <> lnbk
           hline
           mapM_ mkLine (exp ^. parameterSetup)
           hline
           -- label (refTblParamSetting nr)
           -- caption $ "Parameter Setup for experiment nr " <> raw (tshow nr)
  where
    line :: LaTeXC l => Text -> l -> l
    line name value = raw name & value <> lnbk
    mkLine :: LaTeXC l => ParameterSetting a -> l
    mkLine (ParameterSetting n bsV) =
      case find ((== n) . parameterName) (evals ^. evalsExperiments . experimentsParameters) of
        Nothing -> line n (raw "was not modified as it is not listed in the parameter setting")
        Just (ParameterSetup _ setter _ _ (minVal, maxVal)) ->
          case S.runGet S.get bsV of
            Left err -> raw (T.pack err)
            Right val ->
              let _ = setter val (evals ^. evalsExperiments . experimentsInitialState) -- only needed for type inference
              in line n (raw (tshow val) <> math (text " " `in_` autoParens (text (raw (tshow minVal)) <> ", " <> text (raw (tshow maxVal)))))
