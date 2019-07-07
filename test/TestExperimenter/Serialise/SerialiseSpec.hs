{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module TestExperimenter.Serialise.SerialiseSpec (spec) where


import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.Serialize
import qualified Data.Text                            as T
import           GHC.Generics
import           Prelude
import           Test.Hspec
import           Test.QuickCheck

import           Experimenter
import           Experimenter.Result.Query

import           TestExperimenter.Serialise.Instances

spec :: Spec
spec = do
  describe "Serialise and deserialisation properties" $
    do it "serialises & deserialises a start state" $ property prop_serialiseStartSt
       it "serialises & deserialises a end state (Just st)" $ property prop_serialiseEndSt


prop_serialiseStartSt :: St -> Property
prop_serialiseStartSt st = ioProperty $ runStdoutLoggingT $ do
  let bsStart = runPut $ put st
  mSt' <- deserialise (T.pack "prop_serialiseStartSt") bsStart
  return (Just st === mSt')


prop_serialiseEndSt :: Maybe St -> Property
prop_serialiseEndSt mSt = ioProperty $ runStdoutLoggingT $ do
  let bsEnd = runPut . put <$> mSt
  mmSt' <- mDeserialise (T.pack "prop_serialiseEndSt") bsEnd
  return $ (Just <$> mSt) == mmSt'


prop_serialisableEndSt :: Maybe St -> Property
prop_serialisableEndSt mSt = ioProperty $ runStdoutLoggingT $ do
  serESt <- lift $ traverse serialisable mSt
  let bsEnd = runPut . put <$> serESt
  (mmSt' :: Maybe (Maybe St')) <- mDeserialise (T.pack "prop_serialiseEndSt") bsEnd
  mmSt <- lift $ maybe (return Nothing) (fmap Just . deserialisable) (join mmSt')
  return $ mSt == mmSt

data St' = St' Int Double
  deriving (Generic, Serialize)


instance ExperimentDef St where
  type ExpM St = IO
  type Serializable St = St'
  serialisable (St x y) = return $ St' x y
  deserialisable (St' x y) = return $ St x y
  type InputValue St = ()
  type InputState St = ()
  generateInput _ _ _ _ = return ((),())
  runStep (St x y) _ _ = return ([], St x y)
  parameters _ = []

