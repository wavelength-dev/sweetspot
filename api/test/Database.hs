{-# LANGUAGE OverloadedStrings #-}

module Database where

import qualified Data.ByteString as B
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session


migrateUp :: IO ()
migrateUp = do
  sql <- B.readFile "test/migrations/test-data-up.sql"
  Right connection <- Connection.acquire connectionSettings
  Session.run (Session.sql sql) connection
  return ()
  where
    connectionSettings =
      Connection.settings "localhost" 5432 "postgres" "" "sweetspot"

migrateDown :: IO ()
migrateDown = do
  sql <- B.readFile "test/migrations/test-data-down.sql"
  Right connection <- Connection.acquire connectionSettings
  Session.run (Session.sql sql) connection
  return ()
  where
    connectionSettings =
      Connection.settings "localhost" 5432 "postgres" "" "sweetspot"

reset :: IO ()
reset = migrateDown >> migrateUp
