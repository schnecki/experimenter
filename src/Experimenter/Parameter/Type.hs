{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}
module Experimenter.Parameter.Type where

import           Control.DeepSeq
import           Control.Lens
import qualified Data.ByteString as BS
import           Data.Serialize  as S
import qualified Data.Text       as T
import           GHC.Generics


data ExperimentDesign
  = FullFactory
  | SingleInstance
  deriving (Bounded, Enum, Eq, Ord, Generic, NFData)

data ParameterSetup a =
  forall b . (Show b, Ord b, Serialize b) =>
  ParameterSetup
  { parameterName         :: !T.Text                          -- ^ Name of parameter.
  , setParameter          :: !(b -> a -> a)                   -- ^ Set the parameter.
  , getParameter          :: !(a -> b)                        -- ^ Get the parameter from the current state.
  , modifyParameter       :: !(Maybe (b -> IO [b]))           -- ^ Either no modification or function.
  , bounds                :: !(Maybe (b, b))                  -- ^ Bounds (inclusive).
  , skipPreparationPhase  :: !(Maybe (b -> Bool))             -- ^ Skip the preparation phase if True (e.g. to skip learning phase). Default: False.
  , alterExperimentDesign :: !(Maybe (b -> ExperimentDesign)) -- ^ Change the experiment design. Default: Full-Factory design.
  }
-- makeLenses ''ParameterSetup  -- does not work with ExistentialQuantification


data ParameterSetting a =
  ParameterSetting
  { _parameterSettingName                 :: !T.Text
  , _parameterSettingValue                :: !BS.ByteString
  , _parameterSettingSkipPreparationPhase :: !Bool
  , _parameterSettingExperimentDesign     :: !ExperimentDesign
  } deriving (Eq, Generic, NFData)
makeLenses ''ParameterSetting


getParameterData :: ParameterSetup a -> a -> BS.ByteString
getParameterData (ParameterSetup _ _ getter _ _ _ _) a = runPut $ put $ getter a


mkParameterSetting :: ParameterSetup a -> a -> ParameterSetting a
mkParameterSetting (ParameterSetup n _ getter _ _ drp design) a = ParameterSetting n (runPut $ put aVal) (maybe False (\x -> x aVal) drp) (maybe FullFactory (\x -> x aVal) design)
  where aVal = getter a
