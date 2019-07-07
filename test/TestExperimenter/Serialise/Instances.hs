{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module TestExperimenter.Serialise.Instances
    ( St (..)
    ) where


import           Control.DeepSeq
import           Data.Serialize
import           GHC.Generics
import           Test.Hspec
import           Test.QuickCheck


data St = St Int Double
  deriving (Show, Eq, Ord, Generic, Serialize, NFData)


instance Arbitrary St where
  arbitrary = St <$> arbitrary <*> arbitrary

instance CoArbitrary St where
  coarbitrary (St int double) = variant 0 . coarbitrary (int, double)
