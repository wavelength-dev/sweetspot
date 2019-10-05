{-# LANGUAGE OverloadedStrings #-}

module Database where

import qualified Data.ByteString as B
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(..))



connectInfo = ConnectInfo
  { connectHost = "localhost"
  , connectPort = 5432
  , connectUser = "sweetspot"
  , connectPassword = ""
  , connectDatabase = "sweetspot_test"
  }

migrateUp :: IO ()
migrateUp = do
  sql <- B.readFile "test/migrations/test-data-up.sql"
  conn <- connect connectInfo
  execute_ conn (Query sql)
  return ()

migrateDown :: IO ()
migrateDown = do
  sql <- B.readFile "test/migrations/test-data-down.sql"
  conn <- connect connectInfo
  execute_ conn (Query sql)
  return ()

reset :: IO ()
reset = migrateDown >> migrateUp
