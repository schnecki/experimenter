{-# LANGUAGE OverloadedStrings #-}
module Experimenter.Eval.Util where

import           Control.Lens             hiding (Cons, Over, over)
import qualified Data.Text                as T
import           System.FilePath.Posix

import           Experimenter.Eval.Type   as E
import           Experimenter.Result.Type


rootPath :: FilePath
rootPath = "results"

mainFile :: Experiments a -> FilePath
mainFile exps = "main_" <> t <> ".tex"
  where t = maybe "unfinished_experiment" (T.unpack . T.replace " " "_" . T.pack . show) (exps ^. experimentsEndTime)

mainFilePdf :: Experiments a -> FilePath
mainFilePdf exps = T.unpack (T.dropWhileEnd (/= '.') (T.pack $ mainFile exps)) <> "pdf"

getExpsName :: Experiments a -> String
getExpsName exps  = T.unpack $ T.replace " " "_" $ exps ^. experimentsName

expsPath :: Experiments a -> FilePath
expsPath exps = rootPath </> getExpsName exps

