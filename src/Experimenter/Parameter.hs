{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
module Experimenter.Parameter where

import           Control.Lens
import           Data.Serialize
import qualified Data.Text           as T
import           Experimenter.Models
import           System.Random

class (Serialize b, Ord b, Eq b) => ParameterType b where
  type Type b :: *

  defaultModifyParameter :: b -> IO [b]
  default defaultModifyParameter :: (Random b) => b -> IO [b]
  defaultModifyParameter _ = return <$> randomRIO defaultBounds

  defaultBounds :: (b, b)
  {-# MINIMAL defaultBounds #-}


data ParameterSetup a =
  forall b . (Serialize b, ParameterType b) =>
  ParameterSetup
  { _parameterName   :: T.Text           -- ^ Name of parameter.
  , _setParameter    :: b -> a -> a      -- ^ Set the parameter.
  , _getParameter    :: a -> b           -- ^ Get the parameter from the current state.
  , _modifyParameter :: Maybe (b -> IO [b]) -- ^ Either no modification or function.
  , _bounds          :: (b, b)           -- ^ Bounds (inclusive).
  }
makeLenses ''ParameterSetup


instance ParameterType Bool where
  type Type Bool = Bool
  defaultBounds = (False, True)

instance ParameterType Integer where
  type Type Integer = Integer
  defaultBounds = (-1, 1)

instance ParameterType Int where
  type Type Int = Int
  defaultBounds = (-1, 1)

instance ParameterType Double where
  type Type Double = Double
  defaultBounds = (0, 1)

instance ParameterType Float where
  type Type Float = Float
  defaultBounds = (0, 1)


