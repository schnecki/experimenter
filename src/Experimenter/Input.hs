{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}


module Experimenter.Input where

import           Control.Lens

import           Experimenter.Experiment

data Input a = Input
  { _inputValuePeriod :: Integer
  , _inputValue       :: InputValue a
  }

makeLenses ''Input
