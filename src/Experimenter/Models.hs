{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Experimenter.Models where

import           Experimenter.State

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (runStderrLoggingT)
import           Control.Monad.Reader
import           Data.ByteString
import           Data.Text
import           Data.Time                   (UTCTime)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.Quasi
import           Database.Persist.Sql
import           Database.Persist.TH


share [mkPersist sqlSettings {mpsGenerateLenses = True}, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"] $(persistFileWith lowerCaseSettings "config/models")

-- conn = "host=localhost dbname=experimenter user=postgres password=postgres port=5432"

-- getPerson :: MonadIO m => ReaderT SqlBackend m [Entity Person]
-- getPerson = rawSql "select ?? from person where name=?" [PersistText "sibi"]


-- main :: IO ()
-- main = runStderrLoggingT $ withPostgresqlPool conn 10 $ liftSqlPersistMPool $ do
--   runMigration migrateAll
--   xs <- getPerson
--   liftIO (print xs)

-- module Experimenter.Model where

-- import           Database.Persist

-- -- You can define all of your database entities in the entities file.
-- -- You can find more information on persistent and how to declare entities
-- -- at:
-- -- http://www.yesodweb.com/book/persistent/
