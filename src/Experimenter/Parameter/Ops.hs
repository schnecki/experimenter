module Experimenter.Parameter.Ops
    ( convertParameterSetup

    ) where

import           Data.Serialize

import           Experimenter.Models
import           Experimenter.Parameter.Type

convertParameterSetup :: ExpId -> ParameterSetup a -> Param
convertParameterSetup expId (ParameterSetup name _ _ _ (minB,maxB)) = Param expId name (runPut $ put minB) (runPut $ put maxB)
