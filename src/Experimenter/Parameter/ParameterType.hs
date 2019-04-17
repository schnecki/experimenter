{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Experimenter.Parameter.ParameterType where

import           Data.Serialize
import           System.Random

class (Serialize b, Ord b, Eq b) => ParameterType b where
  defaultModifyParameter :: b -> IO [b]
  default defaultModifyParameter :: (Random b) => b -> IO [b]
  defaultModifyParameter _ = return <$> randomRIO defaultBounds

  defaultBounds :: (b, b)
  {-# MINIMAL defaultBounds #-}

instance ParameterType Bool where
  defaultBounds = (False, True)

instance ParameterType Integer where
  defaultBounds = (-1, 1)

instance ParameterType Int where
  defaultBounds = (-1, 1)

instance ParameterType Rational where
  defaultBounds = (0, 1)
  defaultModifyParameter _ =
    let (x :: Rational, y) = defaultBounds
     in return . toRational <$> randomRIO (fromRational x :: Double, fromRational y)

instance ParameterType Double where
  defaultBounds = (0, 1)

instance ParameterType Float where
  defaultBounds = (0, 1)


instance (Eq a, Ord a, Serialize a, Random a, Num a) => ParameterType (Maybe a) where
  defaultBounds = (pure 0, pure 1)
  defaultModifyParameter _ =
    let (fx, fy) = defaultBounds
        fBounds = ((,) <$> fx <*> fy)
     in (\x -> [fail "", x]) <$> sequence (randomRIO <$> fBounds)


-- instance (Eq (f a), Ord (f a), Serialize (f a), Random a, Num a, Monad f, Traversable f) => ParameterType (f a) where
--   defaultBounds = (pure 0, pure 1)
--   defaultModifyParameter _ =
--     let (fx :: f a, fy :: f a) = defaultBounds
--         fBounds = ((,) <$> fx <*> fy)
--      in (\x -> [fail "", x]) <$> sequence (randomRIO <$> fBounds)
