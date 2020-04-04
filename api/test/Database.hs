module Database where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query (..))
import RIO
import qualified RIO.ByteString as B

connectInfo =
  ConnectInfo
    { connectHost = "localhost",
      connectPort = 5432,
      connectUser = "sweetspot",
      connectPassword = "password",
      connectDatabase = "sweetspot"
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
