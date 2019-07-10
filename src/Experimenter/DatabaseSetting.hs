
module Experimenter.DatabaseSetting where

import qualified Data.ByteString as BS

data DatabaseSetting = DatabaseSetting
  { connectionString    :: BS.ByteString -- ^. e.g. "host=localhost dbname=experimenter user=postgres password=postgres port=5432"
  , parallelConnections :: Int
  }
