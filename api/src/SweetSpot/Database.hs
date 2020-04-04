module SweetSpot.Database
  ( Pool,
    getDbPool,
    DbConfig (..),
    migrate,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Pool as P
import qualified Data.Text as T
import Database.Beam.Migrate.Simple
  ( BringUpToDateHooks (..),
    bringUpToDateWithHooks,
  )
import qualified Database.Beam.Postgres as PG
import Database.Beam.Postgres.Migrate (migrationBackend)
import RIO
import SweetSpot.Database.Schema (migration)
import Prelude (print)

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

-- | ---------------------------------------------------------------------------
-- | Migration
-- | ---------------------------------------------------------------------------
verboseHooks :: BringUpToDateHooks PG.Pg
verboseHooks =
  BringUpToDateHooks
    { runIrreversibleHook = pure True,
      startStepHook = \a b ->
        liftIO
          ( print $
              "startStepHook N"
                ++ show a
                ++ ": "
                ++ show b
          ),
      endStepHook = \a b ->
        liftIO
          ( print $
              "endStepHook N"
                ++ show a
                ++ ": "
                ++ show b
          ),
      runCommandHook = \a b ->
        liftIO
          ( print $
              "runCommandHook N"
                ++ show a
                ++ ": "
                ++ show b
          ),
      queryFailedHook = fail "Log entry query fails",
      discontinuousMigrationsHook = \ix ->
        fail
          ( "Discontinuous migration log: missing migration at "
              ++ show ix
          ),
      logMismatchHook = \ix actual expected ->
        fail
          ( "Log mismatch at index "
              ++ show ix
              ++ ":\n"
              ++ "  expected: "
              ++ T.unpack expected
              ++ "\n"
              ++ "  actual  : "
              ++ T.unpack actual
          ),
      databaseAheadHook = \aheadBy ->
        fail
          ( "The database is ahead of the known schema by "
              ++ show aheadBy
              ++ " migration(s)"
          )
    }

migrate conn =
  PG.runBeamPostgres
    conn
    (bringUpToDateWithHooks verboseHooks migrationBackend migration)
