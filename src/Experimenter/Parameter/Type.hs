{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}
module Experimenter.Parameter.Type where

import           Control.Lens
import qualified Data.ByteString as BS
import           Data.Serialize  as S
import qualified Data.Text       as T


data ExperimentDesign
  = FullFactory
  | SingleInstance
  deriving (Bounded, Enum, Eq, Ord)

data ParameterSetup a =
  forall b . (Show b, Ord b, Serialize b) =>
  ParameterSetup
  { parameterName         :: T.Text                        -- ^ Name of parameter.
  , setParameter          :: b -> a -> a                   -- ^ Set the parameter.
  , getParameter          :: a -> b                        -- ^ Get the parameter from the current state.
  , modifyParameter       :: Maybe (b -> IO [b])           -- ^ Either no modification or function.
  , bounds                :: Maybe (b, b)                  -- ^ Bounds (inclusive).
  , dropPreparationPhase  :: Maybe (b -> Bool)             -- ^ Drop the preparation phase if True (e.g. to skip learning phase). Default: False.
  , alterExperimentDesign :: Maybe (b -> ExperimentDesign) -- ^ Change the experiment design. Default: Full-Factory design.
  }
-- makeLenses ''ParameterSetup  -- does not work with ExistentialQuantification


data ParameterSetting a =
  ParameterSetting
  { _parameterSettingName         :: T.Text
  , _parameterSettingValue        :: BS.ByteString
  , _parameterDropPrepeationPhase :: Bool
  , _parameterExperimentDesign    :: ExperimentDesign
  }
makeLenses ''ParameterSetting

getParameterData :: ParameterSetup a -> a -> BS.ByteString
getParameterData (ParameterSetup _ _ getter _ _ _ _) a = runPut $ put $ getter a


mkParameterSetting :: ParameterSetup a -> a -> ParameterSetting a
mkParameterSetting (ParameterSetup n _ getter _ _ drp design) a = ParameterSetting n (runPut $ put aVal) (maybe False (\x -> x aVal) drp) (maybe FullFactory (\x -> x aVal) design)
  where aVal = getter a

