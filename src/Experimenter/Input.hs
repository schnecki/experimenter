{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}


module Experimenter.Input where

import           Control.DeepSeq
import           Control.Lens

import           Experimenter.Experiment

data Input a = Input
  { _inputValuePeriod :: !Int
  , _inputValue       :: !(InputValue a)
  }


instance NFData (InputValue a) => NFData (Input a) where
  rnf (Input p v) = rnf p `seq` rnf v


makeLenses ''Input
