{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Experimenter.Result.Query
    ( loadExperiment
    ) where


import           Control.Lens                (view)
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.List                   as L
import           Data.Serialize              (put, runPut)
import           Data.Time                   (getCurrentTime)
import           Database.Persist
import           Database.Persist.Postgresql (SqlBackend)

import           Experimenter.Experiment
import           Experimenter.Models
import           Experimenter.Parameter
import           Experimenter.Result.Type
import           Experimenter.Setup


loadExperiment :: (ExperimentDef a, MonadLogger m, MonadIO m) => ExperimentSetup -> InputState a -> a -> ReaderT SqlBackend m (Experiment a)
loadExperiment setup initInpSt initSt = do
  eExp <- getOrCreateExp setup initInpSt initSt

  setup <- select

  let e = entityVal eExp
  let experiment = Experiment (entityKey eExp) (view expName e) (view expStartTime e) (view expEndTime e) setup


  undefined

getOrCreateExp :: (ExperimentDef a, MonadLogger m, MonadIO m) => ExperimentSetup -> InputState a -> a -> ReaderT SqlBackend m (Entity Exp)
getOrCreateExp setup initInpSt initSt = do
  let name = view experimentBaseName setup
  exps <- selectList [ExpName ==. name, ExpInitialInputState ==. runPut (put initInpSt), ExpInitialState ==. runPut (put initSt)] []
  params <- mapM (\e -> selectList [ParamExp ==. entityKey e] []) exps
  let mkParamTpl (Param _ n minB maxB) = (n, minB, maxB)
  let myParams = map (mkParamTpl . convertParameterSetup (error "Ref not used")) (parameters initSt)
  case L.find ((== myParams) . map (mkParamTpl . entityVal) . snd) (zip exps params) of
      Nothing -> do
        $(logInfo) "Starting new experiment..."
        time <- liftIO getCurrentTime
        insertEntity $ Exp name time Nothing (runPut $ put initSt) (runPut $ put initInpSt)
      Just (eExp, _) -> do
        $(logInfo) "Found experiment with same name and parameter settings. Continuing experiment ..."
        return eExp
