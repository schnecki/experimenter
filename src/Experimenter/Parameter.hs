{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Experimenter.Parameter where

import           Control.Lens

data Parameter a b = Parameter
  { _setParameter    :: b -> a -> a      -- ^ Set the parameter.
  , _getParameter    :: a -> b           -- ^ Get the parameter from the current state.
  , _modifyParameter :: Maybe (b -> [b]) -- ^ Either no modification or function.
  }


class ParameterType b where
  type Type b :: *
  defaultModifyParameter :: b -> [b]


instance ParameterType Bool where
  type Type Bool = Bool
  defaultModifyParameter = pure . not

instance ParameterType Integer where
  type Type Integer = Integer
  defaultModifyParameter v = [v+1, v-1]

instance ParameterType Int where
  type Type Int = Int
  defaultModifyParameter v = [v+1, v-1]

instance ParameterType Double where
  type Type Double = Double
  defaultModifyParameter v = [v+0.1, v-0.1]


instance ParameterType Float where
  type Type Float = Float
  defaultModifyParameter v = [v+0.1, v-0.1]


