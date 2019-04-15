{-# LANGUAGE OverloadedStrings #-}
module Experimenter.Eval.Latex
    ( writeLatex
    , writeAndCompileLatex
    ) where

import           Control.Lens                 hiding ((&))
import           Control.Monad                (void)
import           Data.Matrix
import           Data.Maybe                   (fromMaybe)
import qualified Data.Text                    as T
import           System.Directory
import           System.Process
import           Text.LaTeX
import           Text.LaTeX.Packages.Inputenc

import           Experimenter.Eval.Type
import           Experimenter.Models
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

compileLatex :: Evals a -> IO ()
compileLatex evals = do
  let n = T.unpack $ evals ^. evalsExperiment.experimentsName
      dir = rootPath <> "/" <> n
  void $ runProcess "pdflatex" [mainFile] (Just dir) Nothing Nothing Nothing Nothing >>= waitForProcess
  pwd <- getCurrentDirectory
  putStrLn $ "\n\nSuccessfully compiled your results! See file://" <> pwd <> "/results/" <> n <> "/" <> mainFilePdf


writeLatex :: Evals a -> IO ()
writeLatex evals = do
  let n = T.unpack $ evals ^. evalsExperiment.experimentsName
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

-- Body with a section.
theBody :: Evals a -> LaTeXT IO ()
theBody evals = do
  maketitle
  experimentsInfo (evals ^. evalsExperiment)
  part "Evaluations"
  mapM_ experimentsEval (evals ^. evalsResults)

  -- section "Hello"
 --  "This is a simple example using the "
 --  hatex
 --  " library. "
 -- -- 'textbf' turns characters to bold font (as you already may know).
 --  textbf "Enjoy!"
 --  " "
 -- -- This is how we nest commands.
 --  textbf (large "Yoohoo!")


experimentsInfo :: Experiments a -> LaTeXT IO ()
experimentsInfo exps = do
  part "General Information"

  -- center $ matrixTabular [] -- (fmap textbf ["","y","z"]) $
  --   (fromList 3 3 [ (1 :: Int)..])

  center $ tabular Nothing [LeftColumn, LeftColumn] $
    hline <>
    line "Experiment Name: " (exps ^. experimentsName) <>
    line "Start time:" (tshow $ exps ^. experimentsStartTime) <>
    line "End time:"  (maybe "" tshow (exps ^. experimentsEndTime)) <>
    line "Number of conducted Experiments: " (tshow $ length (exps ^. experiments)) <>
    line "Experiment Repetitions (complete restarts):" (tshow $ exps ^. experimentsSetup.expsSetupRepetitions) <>
    line "Experiment Preparation Steps:" (tshow $ exps ^. experimentsSetup.expsSetupPreparationSteps) <>
    line "Experiment Evaluation Warm Up Steps:" (tshow $ exps ^. experimentsSetup.expsSetupEvaluationWarmUpSteps) <>
    line "Experiment Evaluation Steps:" (tshow $ exps ^. experimentsSetup.expsSetupEvaluationSteps) <>
    line "Experiment Evaluation Replications:" (tshow $ exps ^. experimentsSetup.expsSetupEvaluationReplications) <>
    hline

  where line name value = raw name & raw value <> lnbk

experimentsEval :: ExperimentEvals a -> LaTeXT IO ()
experimentsEval (ExperimentEval nr res) = do
  pagebreak "4"
  section $ "Experiment " <> raw (tshow nr)
  return ()
