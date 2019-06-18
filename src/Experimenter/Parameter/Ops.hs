module Experimenter.Parameter.Ops
    ( convertParameterSetup

    ) where

import           Data.Serialize

import           Experimenter.Models
import           Experimenter.Parameter.Type

convertParameterSetup :: ExpsId -> ParameterSetup a -> Param
convertParameterSetup expsId (ParameterSetup name _ _ _ (Just (minB,maxB))) = Param expsId name (Just $ runPut $ put minB) (Just $ runPut $ put maxB)
convertParameterSetup expsId (ParameterSetup name _ _ _ Nothing) = Param expsId name Nothing Nothing
