{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Experimenter.Load
    ( OnlyFinishedExperiments
    , DatabaseSetting (..)
    , loadExperimentsResultsM
    , loadExperimentPrepEndStateIO
    , loadExperimentPrepEndState
    ) where


import           Control.Arrow                (first, second, (&&&), (***))
import           Control.Concurrent.MVar
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger         (filterLogger, logDebug, logError,
                                               logInfo, runStdoutLoggingT)
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString              as B
import           Data.Function                (on)
import           Data.Int                     (Int64)
import           Data.IORef
import           Data.List                    (foldl')
import qualified Data.List                    as L
import           Data.Maybe                   (catMaybes, fromJust, fromMaybe,
                                               isJust, isNothing)
import           Data.Serialize               hiding (get)
import qualified Data.Serialize               as S
import qualified Data.Text                    as T
import           Data.Time                    (addUTCTime, diffUTCTime,
                                               getCurrentTime)
import qualified Data.Vector                  as V
import           Database.Persist.Postgresql
import           GHC.Generics
import           Network.HostName             (getHostName)
import           Prelude                      hiding (exp)
import           System.IO
import           System.IO.Unsafe             (unsafePerformIO)
import           System.Posix.Process
import           System.Random.MWC
import           Text.Read                    (readMaybe)

import           Experimenter.Availability
import           Experimenter.DatabaseSetting
import           Experimenter.DB
import           Experimenter.Experiment
import           Experimenter.Input
import           Experimenter.MasterSlave
import           Experimenter.Measure
import           Experimenter.Models
import           Experimenter.Parameter
import           Experimenter.Result
import           Experimenter.Result.Query
import           Experimenter.Setting
import           Experimenter.StepResult
import           Experimenter.Util


type OnlyFinishedExperiments = Bool


loadExperimentsResultsM ::
     (ExperimentDef a)
  => OnlyFinishedExperiments
  -> (ExpM a (Maybe (Experiments a)) -> IO (Maybe (Experiments a)))
  -> DatabaseSetting
  -> MkExperimentSetting a
  -> InputState a
  -> ExpM a a
  -> Int64
  -> IO (Maybe (Experiments a))
loadExperimentsResultsM filtFin runExpM dbSetup setup initInpSt mkInitSt key =
  runExpM $ runDB dbSetup $ do
    initSt <- lift $ lift $ lift mkInitSt
    let sett = setup initSt
        skipPrep exp' = any (^. parameterSettingSkipPreparationPhase) (exp' ^. parameterSetup)
        isFinished exp' =
          length (exp' ^. experimentResults) == sett ^. experimentRepetitions && -- repetitions
          (skipPrep exp' || all (\expRes -> maybe 0 (lengthAvailabilityList . view results) (expRes ^. preparationResults) == sett ^. preparationSteps) (exp' ^. experimentResults)) && -- preparation length
          all (\expRes -> length (expRes ^. evaluationResults) == sett ^. evaluationReplications) (exp' ^. experimentResults) && -- replications
          all
            (\expRes -> maybe 0 (lengthAvailabilityList . view results) (expRes ^. warmUpResults) == sett ^. evaluationWarmUpSteps)
            (exp' ^. experimentResults . traversed . evaluationResults) && -- warm up length
          all
            (\expRes -> maybe 0 (lengthAvailabilityList . view results) (expRes ^. evalResults) == sett ^. evaluationSteps)
            (exp' ^. experimentResults . traversed . evaluationResults) -- eval length
        filterFinished =
          over
            experiments
            (if filtFin
               then filter isFinished
               else id)
    fmap filterFinished <$> loadExperimentsResults sett initInpSt initSt (toSqlKey key)


readValueSafe :: (Read a) => IO (Maybe a)
readValueSafe = do
  liftIO $ putStr "Enter value: " >> hFlush stdout
  readMaybe <$> getLine


readListValueSafeName :: [(a, T.Text)] -> IO (Maybe a)
readListValueSafeName xs = do
  mapM_ (\(nr, (_, n)) -> putStrLn $ show nr <> ":\t " <> T.unpack n) $ zip [(0 :: Int) ..] xs
  mNr <- readValueSafe
  return $
    case mNr of
      Just nr
        | nr >= 0 && nr < length xs -> Just $ fst $ xs !! nr
      _ -> Nothing


-- -- | Convert a 'Maybe' computation to 'MaybeT'.
-- hoistMaybe :: (Applicative m) => Maybe b -> MaybeT m b
-- hoistMaybe = MaybeT . pure


loadExperimentPrepEndStateIO :: (ExpM a ~ IO, ExperimentDef a) => DatabaseSetting -> IO (Maybe a)
loadExperimentPrepEndStateIO = loadExperimentPrepEndState id


loadExperimentPrepEndState ::
     forall a. (ExperimentDef a)
  => (ExpM a (Maybe a) -> IO (Maybe a))
  -> DatabaseSetting
  -> IO (Maybe a)
loadExperimentPrepEndState runExpM dbSetup =
  runExpM $
  runDB dbSetup $
  runMaybeT $ do
    liftIO $ putStrLn "From which experiment do you want to load the data? "
    exps <- lift $ selectList [] []
    let expsNames :: [(Key Exps, T.Text)]
        expsNames = map (entityKey &&& (^. expsName) . entityVal) exps
    (expsKey :: Key Exps) <- MaybeT $ liftIO $ readListValueSafeName expsNames
    exp <- lift $ selectList [ExpExps ==. expsKey] []
    (expKey :: Key Exp) <-
      if length exp == 1
        then return $ entityKey $ head exp
        else do
          liftIO $ putStrLn "Which experiment number?"
          let expNrs = sortOnSnd $ map (entityKey &&& tshow . (^. expNumber) . entityVal) exp
          MaybeT $ liftIO $ readListValueSafeName expNrs
    reps <- lift $ selectList [ExpResultExp ==. expKey] []
    liftIO $ putStrLn "And from which experiment repetition?"

    let
      repNames :: [(Key PrepResultData, T.Text)]
      repNames = sortOnSnd $ map (first fromJust) $ filter (isJust . fst) $ map ((^. expResultPrepResultData) . entityVal &&& tshow . (^. expResultRepetition) . entityVal) reps
    rep <- MaybeT $ liftIO $ readListValueSafeName repNames
    let endStateType = EndStatePrep rep
    ma <- lift (loadResDataEndState expKey endStateType :: DB (ExpM a) (Maybe a))
    hoistMaybe ma
  where sortOnSnd = L.sortBy (compare `on` snd)
