module Experimenter.Util where

import qualified Data.Text as T

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show
