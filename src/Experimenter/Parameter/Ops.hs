module Experimenter.Parameter.Ops
    ( convertParameterSetup

    ) where

import           Data.Serialize

import           Experimenter.Models
import           Experimenter.Parameter.Type

convertParameterSetup :: ExpsId -> ParameterSetup a -> Param
convertParameterSetup expsId (ParameterSetup name _ _ _ (minB,maxB)) = Param expsId name (runPut $ put minB) (runPut $ put maxB)
