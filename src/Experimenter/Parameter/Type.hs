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

-- | Class of Serializable parameter values. In case there is a `Serialize` instance this class instance is
-- automatically inferred. Otherwise a lookup in a list could be performed by manually implementing the instance.
class SerializableParamValue b where

  serializeParamValue :: b -> BS.ByteString
  default serializeParamValue :: (Serialize b) => b -> BS.ByteString
  serializeParamValue = runPut . put

  deserializeParamValue :: BS.ByteString -> Either String b
  default deserializeParamValue :: (Serialize b) => BS.ByteString -> Either String b
  deserializeParamValue = S.runGet S.get


instance Serialize a => SerializableParamValue a


data ParameterSetup a =
  forall b . (Show b, Ord b, SerializableParamValue b) =>
  ParameterSetup
  { parameterName        :: T.Text              -- ^ Name of parameter.
  , setParameter         :: b -> a -> a         -- ^ Set the parameter.
  , getParameter         :: a -> b              -- ^ Get the parameter from the current state.
  , modifyParameter      :: Maybe (b -> IO [b]) -- ^ Either no modification or function.
  , bounds               :: Maybe (b, b)        -- ^ Bounds (inclusive).
  , dropPreparationPhase :: b -> Bool           -- ^ Drop the preparation phase if any of the parameter has this value
                                                -- set to True.
  }
-- makeLenses ''ParameterSetup  -- does not work with ExistentialQuantification


data ParameterSetting a =
  ParameterSetting
  { _parameterSettingName         :: T.Text
  , _parameterSettingValue        :: BS.ByteString
  , _parameterDropPrepeationPhase :: Bool
  }
makeLenses ''ParameterSetting

getParameterData :: ParameterSetup a -> a -> BS.ByteString
getParameterData (ParameterSetup _ _ getter _ _ _) a = serializeParamValue $ getter a


mkParameterSetting :: ParameterSetup a -> a -> ParameterSetting a
mkParameterSetting (ParameterSetup n _ getter _ _ drp) a = ParameterSetting n (serializeParamValue $ getter a) (drp $ getter a)

