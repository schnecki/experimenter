{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Experimenter.Eval.Csv
    ( writeCsvMeasureIO
    , writeCsvMeasure
    , Smoothing (..)
    , MeasureName
    ) where

import           Control.Arrow                ((&&&), (***))
import           Control.Lens                 hiding (Cons, Over)
import           Control.Monad                (unless)
import           Control.Monad.Logger         (logDebug)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Function                (on)
import           Data.List                    (find, foldl', sortBy)
import           Data.Maybe                   (fromMaybe)
import qualified Data.Text                    as T
import qualified Database.Esqueleto           as E
import           Database.Persist.Postgresql  (SqlBackend, runSqlConn, withPostgresqlConn)
import           System.Directory
import           System.FilePath.Posix

import           Experimenter.DatabaseSetting
import           Experimenter.Eval.Reduce
import           Experimenter.Eval.Type       as E
import           Experimenter.Eval.Util
import           Experimenter.Experiment
import           Experimenter.Measure
import           Experimenter.Models
import           Experimenter.Result.Type
import           Experimenter.StepResult
import           Experimenter.Util


data Smoothing = NoSmoothing | SmoothMovAvg Int

type MeasureName = T.Text

writeCsvMeasureIO :: (ExperimentDef a, ExpM a ~ IO) => DatabaseSetting -> Experiments a -> Smoothing -> [MeasureName] -> IO ()
writeCsvMeasureIO = writeCsvMeasure id

writeCsvMeasure :: (ExperimentDef a) => (ExpM a () -> IO ()) -> DatabaseSetting -> Experiments a -> Smoothing -> [MeasureName] -> IO ()
writeCsvMeasure runExpM dbSetup exps smoothing measures = runner runExpM dbSetup exps smoothing measures

runner :: (ExperimentDef a) => (ExpM a () -> IO ()) -> DatabaseSetting -> Experiments a -> Smoothing -> [MeasureName] -> IO ()
runner runExpM dbSetup exps smoothing measures =
  runExpM $
  (runStdoutLoggingT . filterLogger (\s _ -> s /= "SQL")) $ withPostgresqlConn (connectionString dbSetup) $ \backend -> flip runSqlConn backend $
  mapM_ (smoothAndWriteFile exps smoothing) measures

smoothAndWriteFile :: (ExperimentDef a) => Experiments a -> Smoothing -> MeasureName -> ReaderT SqlBackend (LoggingT (ExpM a)) ()
smoothAndWriteFile exps smoothing measureName =
  mapM_ (smoothAndWriteFileExp exps smoothing measureName) (exps ^. experiments)


smoothAndWriteFileExp :: (ExperimentDef a) => Experiments a -> Smoothing -> MeasureName -> Experiment a -> ReaderT SqlBackend (LoggingT (ExpM a)) ()
smoothAndWriteFileExp exps smoothing measureName exp = do
  mapM_
    (\expRes -> do
       mapM_
         (\repRes -> smoothAndWriteFileResultData exps (namePrefix expNr PreparationPhase (None (expRes ^. repetitionNumber) Nothing)) smoothing measureName repRes)
         (expRes ^.. preparationResults . traversed)
       -- mapM_
       --   (\replRes -> do
       --      mapM_
       --        (\repRes ->
       --           smoothAndWriteFileResultData exps (namePrefix expNr WarmUpPhase (None (expRes ^. repetitionNumber) (Just $ replRes ^. replicationNumber))) smoothing measureName repRes)
       --        (replRes ^.. warmUpResults . traversed)
       --      mapM_
       --        (\repRes ->
       --           smoothAndWriteFileResultData exps (namePrefix expNr EvaluationPhase (None (expRes ^. repetitionNumber) (Just $ replRes ^. replicationNumber))) smoothing measureName repRes)
       --        (replRes ^.. evalResults . traversed))
         -- (expRes ^. evaluationResults)
    )
    (exp ^.. experimentResults . traversed)
  -- avg over experiments
  -- when (length (exp ^. experimentResults) > 1) $ do undefined
  where
    expNr = exp ^. experimentNumber


smooth :: Smoothing -> [Measure] -> [Measure]
smooth NoSmoothing xs = xs
smooth (SmoothMovAvg nr) xs = reverse $ snd $ foldl' (\(lastNr, res) x -> (x:lastNr, movAvg (take nr $ x:lastNr) : res)) ([],[]) xs

movAvg :: [Measure] -> Measure
movAvg [] = error "empty list when makeing movAvg"
movAvg xs@(x:_) = set (measureResults.traversed.resultYValue) (Prelude.sum (concatMap (^.. (measureResults.traversed.resultYValue)) xs) / fromIntegral (length xs)) x


smoothAndWriteFileResultData :: (ExperimentDef a) => Experiments a -> T.Text -> Smoothing -> MeasureName -> ResultData a -> ReaderT SqlBackend (LoggingT (ExpM a)) ()
smoothAndWriteFileResultData exps prefix smoothing measureName resData = do
  -- let filterMeasure = over measureResults (filter ((== measureName) . view resultName))
  -- xs <- fmap (map filterMeasure) $ mkAvailableList (resData ^. results) >>= mkTransientlyAvailable . snd
  let len = lengthAvailabilityList (resData ^. results)
  let froms = [0,splitSize..len+1] ++ [len+1 | len `mod` splitSize /= 0]
  let fromTos = zip froms (tail $ map (subtract 1) froms)
  mapM_ (smoothAndWriteFileResultData' exps prefix smoothing measureName resData) fromTos
  where splitSize = 250000

smoothAndWriteFileResultData' :: (ExperimentDef a) => Experiments a -> T.Text -> Smoothing -> MeasureName -> ResultData a -> (Int,Int) -> ReaderT SqlBackend (LoggingT (ExpM a)) ()
smoothAndWriteFileResultData' exps prefix smoothing measureName resData (from, to) = do
  res <- case resData ^. resultDataKey of
    ResultDataPrep key ->
      E.select $
      E.from $ \(measure `E.InnerJoin` result) -> do
        E.on (measure E.^. PrepMeasureId E.==. result E.^. PrepResultStepMeasure)
        E.where_ (measure E.^. PrepMeasurePrepResultData E.==. E.val key)
        E.where_ (result E.^. PrepResultStepName E.==. E.val measureName)
        E.where_ (measure E.^. PrepMeasurePeriod E.>=. E.val from)
        E.where_ (measure E.^. PrepMeasurePeriod E.<=. E.val to)
        E.orderBy [E.asc (measure E.^. PrepMeasurePeriod)]
        return (measure E.^. PrepMeasurePeriod, result E.^. PrepResultStepYValue)
    ResultDataWarmUp key -> undefined
    ResultDataRep key -> undefined
  let toMeasure (E.Value p, E.Value v) = Measure p [StepResult measureName Nothing v]
  let ls = filter (not . null) $ map toFileCts $ smooth smoothing $ map toMeasure res
  when (from == 0) $ do
    liftIO $ putStrLn $ "Processing measure " <> T.unpack measureName <> ". Saving data to: " <> folder
    liftIO $ createDirectoryIfMissing True folder
    liftIO $ writeFile filePath header >> writeFile filePathPlotSh plotSh
  liftIO $ appendFile filePath (unlines ls)
  where
    filePath = folder </> T.unpack (prefix <> "_" <> measureName <> ".csv")
    filePathPlotSh = folder </> "plot.sh"
    folder = expsPath exps </> "csv"

    header = "Period\t" <> T.unpack (prefix <> "_" <> measureName) <> "\n"
    toFileCts (Measure p []) = []
    toFileCts (Measure p [res]) = show p <> "\t" <> show (res ^. resultYValue)
    toFileCts (Measure p _) = error $ "The measure " <> T.unpack measureName <> " has more than one results in period " <> show p


namePrefix :: Int -> Phase -> Avg -> T.Text
namePrefix expNr ph av = "exp" <> tshow expNr <> "_" <> phaseName ph <> "_" <> avgName av <> "_"

phaseName :: Phase -> T.Text
phaseName PreparationPhase = "prep"
phaseName WarmUpPhase      = "warmUp"
phaseName EvaluationPhase  = "eval"

type ReplNr = Int
type RepetNr = Int
data Avg = None RepetNr (Maybe ReplNr) | Repl RepetNr | Repet ReplNr | RepetRepl

avgName :: Avg -> T.Text
avgName (None repet (Just repl)) = "repet" <> tshow repet <> "_repl" <> tshow repl
avgName (None repet Nothing)     = "repet" <> tshow repet
avgName (Repl repet)             = "repet" <> tshow repet <> "_replAvg"
avgName (Repet repl)             = "repetAvg_repl" <> tshow repl
avgName RepetRepl                = "repetAvg_replAvg"

plotSh :: String
plotSh =
  unlines
    [ "FILES=\"\"                                               "
    , "                                                         "
    , "for arg in $@; do                                        "
    , "     FILES=\"$FILES `find . -type f -name \"*$arg*\"`\"  "
    , "done                                                     "
    , "                                                         "
    , "echo $FILES                                              "
    , "                                                         "
    , "ARRAY=($FILES)                                           "
    , "for col in {2,3,4}; do                                   "
    , "    CMD=\"set key autotitle columnhead; plot \"          "
    , "    for f in $FILES; do                                  "
    , "        echo $f                                          "
    , "        CMD=\"$CMD '$f' using 0:$col with lines \"       "
    , "        if [ \"$f\" != \"${ARRAY[-1]}\" ]; then          "
    , "            CMD=\"$CMD, \"                               "
    , "        fi                                               "
    , "    done                                                 "
    , "    CMD=\"$CMD; pause mouse close; \"                    "
    , "    echo $CMD                                            "
    , "    gnuplot -e \"$CMD\" &                                "
    , "done                                                     "
    ]
