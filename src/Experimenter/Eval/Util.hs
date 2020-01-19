{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Experimenter.Eval.Util where

import           Control.DeepSeq
import           Control.Lens             hiding (Cons, Over, over)
import           Control.Monad.IO.Class
import qualified Data.Text                as T
import           Data.Time.Clock          (diffUTCTime, getCurrentTime)
import           System.FilePath.Posix

import           Experimenter.Result.Type


rootPath :: FilePath
rootPath = "results"

mainFile :: Experiments a -> FilePath
mainFile exps = "main_" <> getTime exps <> ".tex"

getTime :: Experiments a -> String
getTime exps = maybe "unfinished_experiment" (T.unpack . T.replace " " "_" . T.pack . show) (exps ^. experimentsEndTime)

scalarFile :: Experiments a -> FilePath
scalarFile exps = "scalar_" <> getTime exps <> ".tex"

repetitionFile :: Experiments a -> FilePath
repetitionFile exps = "repetition_" <> getTime exps <> ".tex"

replicationFile :: Experiments a -> FilePath
replicationFile exps = "replication_" <> getTime exps <> ".tex"

periodicFile :: Experiments a -> FilePath
periodicFile exps = "periodic_" <> getTime exps <> ".tex"

mainFilePdf :: Experiments a -> FilePath
mainFilePdf exps = T.unpack (T.dropWhileEnd (/= '.') (T.pack $ mainFile exps)) <> "pdf"

getExpsName :: Experiments a -> String
getExpsName exps  = T.unpack $ T.replace "/" "_" $ T.replace " " "_" $ exps ^. experimentsName

expsPath :: Experiments a -> FilePath
expsPath exps = rootPath </> getExpsName exps

mkTime :: (MonadIO m, NFData t) => String -> m t -> m t
mkTime name a = do
  start <- liftIO getCurrentTime
  !val <- force <$> a
  end <- liftIO getCurrentTime
  liftIO $ putStrLn (name <> " Computation Time: " ++ show (diffUTCTime end start))
  return val
