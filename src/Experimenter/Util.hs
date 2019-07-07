module Experimenter.Util where

import           Control.Monad
import qualified Data.Text         as T
import qualified Data.Vector       as V
import           System.Random.MWC


tshow :: (Show a) => a -> T.Text
tshow = T.pack . show


tread :: (Read a) => T.Text -> a
tread = read . T.unpack


fromEither :: b -> Either a b -> b
fromEither def Left{}  = def
fromEither _ (Right b) = b


