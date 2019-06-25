module Experimenter.Parameter.Ops
    ( convertParameterSetup
    ) where

import           Experimenter.Models
import           Experimenter.Parameter.Type

convertParameterSetup :: ExpsId -> ParameterSetup a -> Param
convertParameterSetup expsId (ParameterSetup name _ _ _ (Just (minB,maxB)) _) = Param expsId name (Just $ serializeParamValue minB) (Just $ serializeParamValue maxB)
convertParameterSetup expsId (ParameterSetup name _ _ _ Nothing _) = Param expsId name Nothing Nothing
