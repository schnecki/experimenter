{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Experimenter.Eval.Latex
    ( writeLatex
    , writeAndCompileLatex
    ) where

import           Control.Lens                 hiding ((&))
import           Control.Monad                (void)
import           Data.List                    (find)
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

rootPath :: FilePath
rootPath = "results"

mainFile :: FilePath
mainFile = "main.tex"

mainFilePdf :: FilePath
mainFilePdf = T.unpack (T.dropWhileEnd (/= '.') (T.pack mainFile)) <> "pdf"

writeAndCompileLatex :: Evals a -> IO ()
writeAndCompileLatex evals = writeLatex evals >> compileLatex evals

getExpsName :: Evals a -> String
getExpsName evals  = T.unpack $ T.replace " " "_" $ evals ^. evalsExperiment.experimentsName

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
  execLaTeXT (root evals) >>= renderFile file

root :: Evals a -> LaTeXT IO ()
root evals = do
  thePreamble evals
  document $ theBody evals

-- Preamble with some basic info.
thePreamble :: Evals a -> LaTeXT IO ()
thePreamble evals = do
  let n = evals ^. evalsExperiment.experimentsName
  documentclass [] article
  author "Manuel Schneckenreither"
  title $ "Evaluation for ``" <> raw n <> "''"
  usepackage [utf8] inputenc
  usepackage [] "fullpage"
  usepackage [] "array"
  usepackage [] amsmath

-- Body with a section.
theBody :: Evals a -> LaTeXT IO ()
theBody evals = do
  maketitle
  experimentsInfo (evals ^. evalsExperiment)
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

experimentsInfo :: Experiments a -> LaTeXT IO ()
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

experimentsEval :: Evals a -> ExperimentEval a -> LaTeXT IO ()
experimentsEval evals eval@(ExperimentEval nr res _) = do
  pagebreak "4"
  section $ "Experiment " <> raw (tshow nr)
  paramSetting evals eval
  return ()


paramSetting :: Evals a -> ExperimentEval a -> LaTeXT IO ()
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
      case find ((== n) . parameterName) (evals ^. evalsExperiment . experimentsParameters) of
        Nothing -> line n (raw "was not modified as it is not listed in the parameter setting")
        Just (ParameterSetup _ setter _ _ (minVal, maxVal)) ->
          case S.runGet S.get bsV of
            Left err -> raw (T.pack err)
            Right val ->
              let _ = setter val (evals ^. evalsExperiment . experimentsInitialState) -- only needed for type inference
              in line n (raw (tshow val) <> math (text " " `in_` autoParens (text (raw (tshow minVal)) <> ", " <> text (raw (tshow maxVal)))))
