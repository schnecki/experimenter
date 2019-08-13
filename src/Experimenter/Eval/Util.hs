{-# LANGUAGE OverloadedStrings #-}
module Experimenter.Eval.Util where

import           Control.Lens             hiding (Cons, Over, over)
import qualified Data.Text                as T

import           Experimenter.Eval.Type   as E
import           Experimenter.Result.Type


rootPath :: FilePath
rootPath = "results"

mainFile :: Evals a -> FilePath
mainFile evals = "main_" <> t <> ".tex"
  where t = maybe "unfinished_experiment" (T.unpack . T.replace " " "_" . T.pack . show) (evals ^. evalsExperiments.experimentsEndTime)

mainFilePdf :: Evals a -> FilePath
mainFilePdf evals = T.unpack (T.dropWhileEnd (/= '.') (T.pack $ mainFile evals)) <> "pdf"


getExpsName :: Evals a -> String
getExpsName evals  = T.unpack $ T.replace " " "_" $ evals ^. evalsExperiments.experimentsName
