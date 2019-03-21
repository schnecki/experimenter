{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies      #-}
module Experimenter.ParameterType where

import           Data.Serialize
import           System.Random

class (Serialize b, Ord b, Eq b) => ParameterType b where
  type Type b :: *

  defaultModifyParameter :: b -> IO [b]
  default defaultModifyParameter :: (Random b) => b -> IO [b]
  defaultModifyParameter _ = return <$> randomRIO defaultBounds

  defaultBounds :: (b, b)
  {-# MINIMAL defaultBounds #-}

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


