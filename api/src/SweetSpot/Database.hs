{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module SweetSpot.Database
        ( Pool
        , getDbPool
        , getNewDbPool
        , getExperimentBuckets
        , getCampaignStats
        , DbConfig(..)
        , migrate
        )
where

import           Control.Monad.IO.Class         ( liftIO )
import qualified Database.Beam.Postgres        as PG
import           Database.Beam.Migrate.Simple   ( bringUpToDateWithHooks
                                                , BringUpToDateHooks(..)
                                                )
import           Database.Beam.Postgres.Migrate ( migrationBackend )
import           Control.Lens                   ( _Left
                                                , over
                                                )
import           Data.ByteString.UTF8           ( fromString )
import qualified Data.Text                     as T
import qualified Data.Pool                     as P
import qualified Hasql.Connection              as Conn
import qualified Hasql.Pool                    as Pool

import           SweetSpot.Data.Api
import           SweetSpot.Data.Common
import           SweetSpot.Data.Domain          ( DBCampaignStats )
import           SweetSpot.Database.Sessions
import           SweetSpot.Database.Schema      ( migration )

type Pool = Pool.Pool

data DbConfig = DbConfig
  { host :: String
  , port :: Int
  , name :: String
  , user :: String
  , password :: String
  }

getDbPool :: DbConfig -> IO Pool
getDbPool DbConfig {..} = Pool.acquire
        (poolSize, timeoutMs, connectionSettings)
    where
        poolSize           = 10
        timeoutMs          = 2000
        connectionSettings = Conn.settings (fromString host)
                                           (fromIntegral port)
                                           (fromString user)
                                           (fromString password)
                                           (fromString name)

getNewDbPool :: DbConfig -> IO (P.Pool PG.Connection)
getNewDbPool DbConfig {..} = P.createPool initConn
                                          PG.close
                                          subPools
                                          timeoutMs
                                          poolSize
    where
        initConn = PG.connect
                (PG.ConnectInfo { connectHost     = host
                                , connectPort     = fromIntegral port
                                , connectUser     = user
                                , connectPassword = password
                                , connectDatabase = name
                                }
                )
        subPools  = 1
        timeoutMs = 2000
        poolSize  = 10

wrapQueryError :: Pool.UsageError -> T.Text
wrapQueryError = T.pack . show

getExperimentBuckets :: Pool -> IO (Either T.Text [ExperimentBuckets])
getExperimentBuckets pool = do
        res <- Pool.use pool getBucketsSession
        return $ over _Left wrapQueryError res

getCampaignStats :: Pool -> CampaignId -> IO (Either T.Text DBCampaignStats)
getCampaignStats pool cmpId = do
        res <- Pool.use pool (getCampaignStatsSession cmpId)
        return $ over _Left wrapQueryError res


-- | ---------------------------------------------------------------------------
-- | Migration
-- | ---------------------------------------------------------------------------
verboseHooks :: BringUpToDateHooks PG.Pg
verboseHooks = BringUpToDateHooks
        { runIrreversibleHook         = pure True
        , startStepHook               =
                \a b ->
                        liftIO
                                (  print
                                $  "startStepHook N"
                                ++ show a
                                ++ ": "
                                ++ show b
                                )
        , endStepHook                 =
                \a b ->
                        liftIO
                                (  print
                                $  "endStepHook N"
                                ++ show a
                                ++ ": "
                                ++ show b
                                )
        , runCommandHook              =
                \a b ->
                        liftIO
                                (  print
                                $  "runCommandHook N"
                                ++ show a
                                ++ ": "
                                ++ show b
                                )
        , queryFailedHook             = fail "Log entry query fails"
        , discontinuousMigrationsHook =
                \ix -> fail
                        (  "Discontinuous migration log: missing migration at "
                        ++ show ix
                        )
        , logMismatchHook             = \ix actual expected -> fail
                                                (  "Log mismatch at index "
                                                ++ show ix
                                                ++ ":\n"
                                                ++ "  expected: "
                                                ++ T.unpack expected
                                                ++ "\n"
                                                ++ "  actual  : "
                                                ++ T.unpack actual
                                                )
        , databaseAheadHook           =
                \aheadBy -> fail
                        (  "The database is ahead of the known schema by "
                        ++ show aheadBy
                        ++ " migration(s)"
                        )
        }

migrate conn = PG.runBeamPostgres
        conn
        (bringUpToDateWithHooks verboseHooks migrationBackend migration)
