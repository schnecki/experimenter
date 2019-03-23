{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}


module Experimenter.Input where

import           Control.Lens

import           Experimenter.Experiment

data Input a = Input
  { _inputValuePeriod :: Int
  , _inputValue       :: InputValue a
  }

makeLenses ''Input
