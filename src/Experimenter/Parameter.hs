{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
module Experimenter.Parameter where

import           Data.Serialize
import qualified Data.Text                  as T

import           Experimenter.ParameterType


data ParameterSetup a =
  forall b . (Serialize b, ParameterType b) =>
  ParameterSetup
  { parameterName   :: T.Text           -- ^ Name of parameter.
  , setParameter    :: b -> a -> a      -- ^ Set the parameter.
  , getParameter    :: a -> b           -- ^ Get the parameter from the current state.
  , modifyParameter :: Maybe (b -> IO [b]) -- ^ Either no modification or function.
  , bounds          :: (b, b)           -- ^ Bounds (inclusive).
  }
-- makeLenses ''ParameterSetup  -- does not work with ExistentialQuantification
