{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
module Experimenter.Parameter.Type where

import           Control.Lens
import qualified Data.ByteString                      as BS
import           Data.Serialize
import qualified Data.Text                            as T

import           Experimenter.Parameter.ParameterType


data ParameterSetup a =
  forall b . (Show b, Ord b, Serialize b, ParameterType b) =>
  ParameterSetup
  { parameterName   :: T.Text              -- ^ Name of parameter.
  , setParameter    :: b -> a -> a         -- ^ Set the parameter.
  , getParameter    :: a -> b              -- ^ Get the parameter from the current state.
  , modifyParameter :: Maybe (b -> IO [b]) -- ^ Either no modification or function.
  , bounds          :: (b, b)              -- ^ Bounds (inclusive).
  }
-- makeLenses ''ParameterSetup  -- does not work with ExistentialQuantification


data ParameterSetting a =
  ParameterSetting
  { _parameterSettingName  :: T.Text
  , _parameterSettingValue :: BS.ByteString
  }
makeLenses ''ParameterSetting

getParameterData :: ParameterSetup a -> a -> BS.ByteString
getParameterData (ParameterSetup _ _ getter _ _) a = runPut $ put $ getter a


mkParameterSetting :: ParameterSetup a -> a -> ParameterSetting a
mkParameterSetting (ParameterSetup n _ getter _ _) a = ParameterSetting n (runPut $ put $ getter a)

