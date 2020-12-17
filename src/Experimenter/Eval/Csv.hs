{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
module Experimenter.Eval.Csv
    ( writeCsvMeasure
    , Smoothing (..)
    , MeasureName
    ) where

import           Conduit                      as C
import           Control.Lens                 hiding (Cons, Over)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Maybe                   (fromMaybe)
import qualified Data.Text                    as T
import           Data.Time.Clock              (diffUTCTime, getCurrentTime)
import qualified Database.Esqueleto           as E
import           System.Directory
import           System.FilePath.Posix
import           System.IO

import           Experimenter.DatabaseSetting
import           Experimenter.DB
import           Experimenter.Eval.Util
import           Experimenter.Experiment      (Phase (..))
import           Experimenter.Models
import           Experimenter.Result.Type
import           Experimenter.Util


data Smoothing = NoSmoothing | SmoothingMovAvg !Int

type MeasureName = T.Text

writeCsvMeasure :: DatabaseSetting -> Experiments a -> Smoothing -> [MeasureName] -> IO ()
writeCsvMeasure dbSetup exps smoothing measures = runDBWithM runResourceT dbSetup $ mapM_ (smoothAndWriteFile exps smoothing) measures

smoothAndWriteFile :: Experiments a -> Smoothing -> MeasureName -> DB IO ()
smoothAndWriteFile exps smoothing measureName = mapM_ (smoothAndWriteFileExp exps smoothing measureName) (exps ^. experiments)

smoothAndWriteFileExp :: Experiments a -> Smoothing -> MeasureName -> Experiment a -> DB IO ()
smoothAndWriteFileExp exps smoothing measureName exp = do
  $(logDebug) $ "Processing CSV for experiment number " <> tshow (exp ^. experimentNumber)
  mapM_
    (\expRes -> do
       $(logDebug) $ "Processing CSV for experiment repetition number " <> tshow (expRes ^. repetitionNumber)
       mapM_
         (smoothAndWriteFileResultData exps (namePrefix expNr PreparationPhase (None (expRes ^. repetitionNumber) Nothing)) smoothing measureName . return)
         (expRes ^.. preparationResults . traversed)
       -- All experiment evaluations runs
       mapM_
         (\(replRes :: ReplicationResult a) -> do
            mapM_
              (\(repRes :: ResultData a) ->
                 smoothAndWriteFileResultData
                   exps
                   (namePrefix expNr WarmUpPhase (None (expRes ^. repetitionNumber) (Just $ replRes ^. replicationNumber)))
                   smoothing
                   measureName
                   [repRes])
              (replRes ^.. warmUpResults . traversed)
            mapM_
              (\(repRes :: ResultData a) ->
                 smoothAndWriteFileResultData
                   exps
                   (namePrefix expNr EvaluationPhase (None (expRes ^. repetitionNumber) (Just $ replRes ^. replicationNumber)))
                   smoothing
                   measureName
                   [repRes])
              (replRes ^.. evalResults . traversed))
         (expRes ^. evaluationResults))
    (exp ^.. experimentResults . traversed)
  -- avg over experiments
  -- when (length (exp ^. experimentResults) > 1) $ do
  --   $(logDebug) $ "Processing aggregated CSV for experiment  number " <> tshow (exp ^. experimentNumber)
  --   mapM_
  --     (\expRes -> do
  --         error "Averages over experiment results are currently not supported. Please feel free to submit a merge request."
  --         print expRes
  --         ) (exp ^. experimentResults)
  where
    expNr = exp ^. experimentNumber


smoothC :: (Monad m) => Smoothing -> C.ConduitT (Int, Double) (Int, Double) m ()
smoothC NoSmoothing = C.filterC (const True)
smoothC (SmoothingMovAvg nr) = process (0, [])
  where
    process inp@(sm, xs) = do
      mx <- await
      unless (null xs) $ yield (movAvg inp)
      case mx of
        Nothing -> return ()
        Just (x :: (Int, Double)) -> do
          let m
                | length xs < nr = 0
                | otherwise = snd $ last xs
              v = snd x
          process (sm + v - m, take nr (x : xs))

-- movAvg :: [Measure] -> Measure
-- movAvg [] = error "empty list when makeing movAvg"
-- movAvg xs@(x:_) = set (measureResults.traversed.resultYValue) (Prelude.sum (concatMap (^.. (measureResults.traversed.resultYValue)) xs) / fromIntegral (length xs)) x

movAvg :: (Double, [(Int, Double)]) -> (Int, Double)
movAvg (_,[])   = error "No input for movAvg. Programming error!"
movAvg (sm, xs) = (fst x, sm / fromIntegral (length xs))
  where x = last xs

data Keys = ResultDataPrepKeys ![Key PrepResultData]
          | ResultDataWarmUpKeys ![Key WarmUpResultData]
          | ResultDataRepKeys ![Key RepResultData]

concatKeys :: Keys -> Keys -> Keys
concatKeys (ResultDataPrepKeys xs) (ResultDataPrepKeys ys) = ResultDataPrepKeys (xs ++ ys)
concatKeys (ResultDataWarmUpKeys xs) (ResultDataWarmUpKeys ys)= ResultDataWarmUpKeys (xs ++ ys)
concatKeys (ResultDataRepKeys xs) (ResultDataRepKeys ys) = ResultDataRepKeys (xs ++ ys)
concatKeys _ _ = error "cannot concat different key types in Csv.hs"

smoothAndWriteFileResultData :: Experiments a -> T.Text -> Smoothing -> MeasureName -> [ResultData a] -> DB IO ()
smoothAndWriteFileResultData _ _ _ _ [] = return ()
smoothAndWriteFileResultData exps prefix smoothing measureName resData = do
  $(logInfo) $ "Processing measure " <> measureName <> ". Saving data to: " <> T.pack folder
  liftIO $ createDirectoryIfMissing True folder
  liftIO $ writeFile filePath header >> writeFile filePathPlotSh plotSh
  fileH <- liftIO $ openFile filePath AppendMode
  start <- liftIO getCurrentTime
  let vals =
        -- fmap (map (fromMaybe 0 . E.unValue)) $
        -- E.select $ -- E.selectSource $
        E.selectSource $
        case foldl1 concatKeys keys of
          ResultDataPrepKeys xs ->
            E.from $ \(measure `E.InnerJoin` result) -> do
              E.on (measure E.^. PrepMeasureId E.==. result E.^. PrepResultStepMeasure)
              case xs of
                [x] -> E.where_ (measure E.^. PrepMeasurePrepResultData E.==. E.val x)
                _ -> E.where_ (measure E.^. PrepMeasurePrepResultData `E.in_` E.valList xs)
              E.where_ (result E.^. PrepResultStepName E.==. E.val measureName)
              E.orderBy [E.asc (measure E.^. PrepMeasurePeriod)]
              E.groupBy (measure E.^. PrepMeasurePeriod)
              return (measure E.^. PrepMeasurePeriod, E.avg_ $ result E.^. PrepResultStepYValue)
          ResultDataWarmUpKeys xs ->
            E.from $ \(measure `E.InnerJoin` result) -> do
              E.on (measure E.^. WarmUpMeasureId E.==. result E.^. WarmUpResultStepMeasure)
              case xs of
                [x] -> E.where_ (measure E.^. WarmUpMeasureRepResult E.==. E.val x)
                _ -> E.where_ (measure E.^. WarmUpMeasureRepResult `E.in_` E.valList xs)
              E.where_ (result E.^. WarmUpResultStepName E.==. E.val measureName)
              E.orderBy [E.asc (measure E.^. WarmUpMeasurePeriod)]
              E.groupBy (measure E.^. WarmUpMeasurePeriod)
              return (measure E.^. WarmUpMeasurePeriod, E.avg_ $ result E.^. WarmUpResultStepYValue)
          ResultDataRepKeys xs ->
            E.from $ \(measure `E.InnerJoin` result) -> do
              E.on (measure E.^. RepMeasureId E.==. result E.^. RepResultStepMeasure)
              case xs of
                [x] -> E.where_ (measure E.^. RepMeasureRepResult E.==. E.val x)
                _   -> E.where_ (measure E.^. RepMeasureRepResult `E.in_` E.valList xs)
              E.where_ (result E.^. RepResultStepName E.==. E.val measureName)
              E.orderBy [E.asc (measure E.^. RepMeasurePeriod)]
              E.groupBy (measure E.^. RepMeasurePeriod)
              return (measure E.^. RepMeasurePeriod, E.avg_ $ result E.^. RepResultStepYValue)
      keys = map (toKeys . view resultDataKey) resData
      toKeys (ResultDataRep key)    = ResultDataRepKeys [key]
      toKeys (ResultDataWarmUp key) = ResultDataWarmUpKeys [key]
      toKeys (ResultDataPrep key)   = ResultDataPrepKeys [key]


  -- let toMeasure (E.Value p, E.Value v) = Measure p [StepResult measureName Nothing v]
  C.runConduit $
    vals C..| C.mapC fromValueC C..| smoothC smoothing C..| C.mapC toFileCts C..| C.filterC (not . null) C..| C.mapC (T.pack . (++ "\n")) C..| C.encodeUtf8C C..| sinkHandle fileH

  liftIO $ hFlush fileH >> hClose fileH
  end <- liftIO getCurrentTime
  $(logInfo) $ "Done. Computation Time: " <> tshow (diffUTCTime end start)
  where
    filePath = folder </> T.unpack (prefix <> "_" <> measureName <> ".csv")
    filePathPlotSh = folder </> "plot.sh"
    folder = expsPath exps </> "csv"
    header = "Period\t" <> T.unpack (prefix <> "_" <> measureName) <> "\n"
    toFileCts (p, res) = show p <> "\t" <> show res
    fromValueC :: (E.Value Int, E.Value (Maybe Double)) -> (Int, Double)
    fromValueC (vPeriod, vMVal) = (E.unValue vPeriod, fromMaybe 0 $ E.unValue vMVal)


-- smoothAndWriteFileResultData :: Experiments a -> T.Text -> Smoothing -> MeasureName -> ResultData a -> DB IO ()
-- smoothAndWriteFileResultData exps prefix smoothing measureName resData = do
--   $(logInfo) $ "Processing measure " <> measureName <> ". Saving data to: " <> T.pack folder
--   liftIO $ createDirectoryIfMissing True folder
--   liftIO $ writeFile filePath header >> writeFile filePathPlotSh plotSh
--   fileH <- liftIO $ openFile filePath AppendMode
--   start <- liftIO getCurrentTime
--   let src =
--         E.selectSource $
--         case resData ^. resultDataKey of
--           ResultDataPrep key ->
--             E.from $ \(measure `E.InnerJoin` result) -> do
--               E.on (measure E.^. PrepMeasureId E.==. result E.^. PrepResultStepMeasure)
--               E.where_ (measure E.^. PrepMeasurePrepResultData E.==. E.val key)
--               E.where_ (result E.^. PrepResultStepName E.==. E.val measureName)
--               E.orderBy [E.asc (measure E.^. PrepMeasurePeriod)]
--               return (measure E.^. PrepMeasurePeriod, result E.^. PrepResultStepYValue)
--           ResultDataWarmUp key -> undefined
--           ResultDataRep key -> undefined
--   let toMeasure (E.Value p, E.Value v) = Measure p [StepResult measureName Nothing v]
--   C.runConduit $
--     src C..| C.mapC toMeasure C..| smoothC smoothing C..| C.mapC toFileCts C..| C.filterC (not . null) C..| C.mapC (T.pack . (++ "\n")) C..| C.encodeUtf8C C..| sinkHandle fileH
--   liftIO $ hFlush fileH >> hClose fileH
--   end <- liftIO getCurrentTime
--   $(logInfo) $ "Done. Computation Time: " <> tshow (diffUTCTime end start)
--   where
--     filePath = folder </> T.unpack (prefix <> "_" <> measureName <> ".csv")
--     filePathPlotSh = folder </> "plot.sh"
--     folder = expsPath exps </> "csv"
--     header = "Period\t" <> T.unpack (prefix <> "_" <> measureName) <> "\n"
--     toFileCts (Measure p []) = []
--     toFileCts (Measure p [res]) = show p <> "\t" <> show (res ^. resultYValue)
--     toFileCts (Measure p _) = error $ "The measure " <> T.unpack measureName <> " has more than one results in period " <> show p


namePrefix :: Int -> Phase -> Avg -> T.Text
namePrefix expNr ph av = "exp" <> tshow expNr <> "_" <> phaseName ph <> "_" <> avgName av <> "_"

phaseName :: Phase -> T.Text
phaseName PreparationPhase = "prep"
phaseName WarmUpPhase      = "warmUp"
phaseName EvaluationPhase  = "eval"

type ReplNr = Int
type RepetNr = Int
data Avg = None !RepetNr !(Maybe ReplNr) | Repl !RepetNr | Repet !ReplNr | RepetRepl

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
