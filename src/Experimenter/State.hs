{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Experimenter.State where

import           Data.Aeson
import           Database.Persist.TH
import           GHC.Generics

data ExperimentState = InProgress | Finished
  deriving (Show, Read, Eq, ToJSON, FromJSON, Generic)

derivePersistField "ExperimentState"
