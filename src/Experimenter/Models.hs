{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Experimenter.Models where


import           Data.ByteString
import           Data.Text
import           Data.Time                   (UTCTime)
import           Database.Persist.Postgresql
import           Database.Persist.Quasi
import           Database.Persist.TH


share [mkPersist sqlSettings {mpsGenerateLenses = True}, -- mkDeleteCascade sqlSettings,
       mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")
