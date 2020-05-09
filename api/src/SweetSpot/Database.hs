module SweetSpot.Database
  ( Pool,
    getDbPool,
    DbConfig (..),
    verifyDbSchema,
    migrate,
  )
where

import qualified Data.Pool as P
import Database.Beam.Migrate.Simple
  ( VerificationResult (..),
    verifySchema,
  )
import qualified Database.Beam.Postgres as PG
import Database.Beam.Postgres.Migrate (migrationBackend)
import Database.PostgreSQL.Simple (withTransaction)
import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand (..),
    MigrationContext (..),
    MigrationResult,
    runMigration,
  )
import Database.PostgreSQL.Simple.Util (existsTable)
import RIO
import qualified RIO.Text as T
import SweetSpot.Database.Schema (checkedDb)
import Text.Pretty.Simple (pPrint)

type Pool = P.Pool PG.Connection

data DbConfig
  = DbConfig
      { host :: !T.Text,
        name :: !T.Text,
        password :: !T.Text
      }

getDbPool :: DbConfig -> IO (P.Pool PG.Connection)
getDbPool DbConfig {..} =
  P.createPool
    initConn
    PG.close
    subPools
    timeoutMs
    poolSize
  where
    initConn =
      PG.connect
        ( PG.ConnectInfo
            { connectHost = T.unpack host,
              connectPort = 5432,
              connectUser = "sweetspot",
              connectPassword = T.unpack password,
              connectDatabase = T.unpack name
            }
        )
    subPools = 1
    timeoutMs = 2000
    poolSize = 10

verifyDbSchema :: PG.Connection -> IO ()
verifyDbSchema conn = do
  res <- PG.runBeamPostgres conn (verifySchema migrationBackend checkedDb)
  case res of
    VerificationFailed cs -> do
      pPrint cs
      exitWith (ExitFailure 1)
    VerificationSucceeded -> mempty

migrate :: PG.Connection -> IO (MigrationResult String)
migrate conn = do
  initialized <- existsTable conn "schema_migrations"
  when (not initialized) $ do
    withTransaction conn $ runMigration $ MigrationContext MigrationInitialization True conn
    mempty
  withTransaction conn $ runMigration $ MigrationContext migrationDir True conn
  where
    migrationDir = MigrationDirectory "./migrations"
