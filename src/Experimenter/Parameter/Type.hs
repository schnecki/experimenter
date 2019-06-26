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


data ParameterSetup a =
  forall b . (Show b, Ord b, Serialize b) =>
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
getParameterData (ParameterSetup _ _ getter _ _ _) a = runPut $ put $ getter a


mkParameterSetting :: ParameterSetup a -> a -> ParameterSetting a
mkParameterSetting (ParameterSetup n _ getter _ _ drp) a = ParameterSetting n (runPut $ put $ getter a) (drp $ getter a)

