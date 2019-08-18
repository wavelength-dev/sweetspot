{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module SweetSpot.Database
  ( Pool
  , getDbPool
  , getNewDbPool
  , getUserBuckets
  , createExperiment
  , getNewCampaignBuckets
  , getExperimentBuckets
  , getCampaignStats
  , DbConfig(..)
  , insertEvent
  , insertLogEvent
  , migrate
  , validateCampaign
  ) where

import qualified Database.Beam.Postgres as PG
import Control.Lens (_Left, over)
import Control.Monad (mapM)
import Data.Aeson (Value)
import Data.ByteString.UTF8 (fromString)
import qualified Data.Text as T
import qualified Data.Pool as P
import qualified Hasql.Connection as Conn
import Hasql.Migration
  ( MigrationCommand(..)
  , loadMigrationsFromDirectory
  , runMigration
  )
import qualified Hasql.Pool as Pool
import Hasql.Transaction.Sessions (IsolationLevel(..), Mode(..), transaction)
import SweetSpot.Data.Api
import SweetSpot.Data.Common (EventType(..), Price, Sku, Svid)
import SweetSpot.Data.Common
import SweetSpot.Data.Domain (DBCampaignStats)
import SweetSpot.Database.Sessions
import System.Random (randomRIO)

type Pool = Pool.Pool

data DbConfig = DbConfig
  { host :: String
  , port :: Int
  , name :: String
  , user :: String
  , password :: String
  }

getDbPool :: DbConfig -> IO Pool
getDbPool DbConfig {..} = do
  Pool.acquire (poolSize, timeoutMs, connectionSettings)
  where
    poolSize = 10
    timeoutMs = 2000
    connectionSettings =
      Conn.settings
        (fromString host)
        (fromIntegral port)
        (fromString user)
        (fromString password)
        (fromString name)

getNewDbPool :: DbConfig -> IO (P.Pool PG.Connection)
getNewDbPool DbConfig {..} =
  P.createPool
    connect
    PG.close
    subPools
    timeoutMs
    poolSize
  where
    connect=
      PG.connect (PG.ConnectInfo
                  { connectHost = host
                  , connectPort = fromIntegral port
                  , connectUser = user
                  , connectPassword = password
                  , connectDatabase = name
                  })
    subPools = 1
    timeoutMs = 2000
    poolSize = 10

wrapQueryError :: Pool.UsageError -> T.Text
wrapQueryError = T.pack . show

getUserBuckets :: Pool -> UserId -> IO (Either T.Text [UserBucket])
getUserBuckets pool userId = do
  res <- Pool.use pool (getUserBucketSession userId)
  return $ over _Left wrapQueryError res

getNewCampaignBuckets :: Pool -> CampaignId -> Maybe UserId -> IO (Either T.Text [UserBucket])
getNewCampaignBuckets pool cmpId mUid = do
  randIdx <- randomRIO (0 :: Int, 1 :: Int)
  res <- Pool.use pool (assignAndGetUserBucketSession cmpId mUid (bucketTypes !! randIdx))
  return $ over _Left wrapQueryError res
  where
    bucketTypes = [Control, Test]

getExperimentBuckets :: Pool -> IO (Either T.Text [ExperimentBuckets])
getExperimentBuckets pool = do
  res <- Pool.use pool getBucketsSession
  return $ over _Left wrapQueryError res

insertEvent :: Pool -> (EventType, Value) -> IO (Either T.Text ())
insertEvent pool input = do
  res <- Pool.use pool (insertEventSession input)
  return $ over _Left wrapQueryError res

insertLogEvent :: Pool -> Value -> IO (Either T.Text ())
insertLogEvent pool val = do
  res <- Pool.use pool (insertEventSession input)
  return $ over _Left wrapQueryError res
  where
    input = (Log, val)

createExperiment ::
     Pool
  -> Sku
  -> Svid
  -> Svid
  -> Price
  -> Price
  -> CampaignId
  -> T.Text
  -> IO (Either T.Text ())
createExperiment pool sku contSvid testSvid contPrice testPrice cmp name = do
  res <- Pool.use pool (createExperimentSession (sku, contSvid, testSvid, contPrice, testPrice, cmp, name))
  return $ over _Left wrapQueryError res

getCampaignStats :: Pool -> CampaignId -> IO (Either T.Text DBCampaignStats)
getCampaignStats pool cmpId = do
  res <- Pool.use pool (getCampaignStatsSession cmpId)
  return $ over _Left wrapQueryError res

migrate :: Pool -> IO (Either T.Text ())
migrate pool = do
  ms <- loadMigrationsFromDirectory "migrations"
  tx <- return . fmap sequence . mapM runMigration $ MigrationInitialization : ms
  res <- Pool.use pool (transaction ReadCommitted Write tx)
  return $
    case res of
      Right Nothing -> Right ()
      Right (Just errors) ->
        Left $ T.intercalate ", " (T.pack . show <$> errors)
      Left err -> Left $ wrapQueryError err

validateCampaign :: Pool -> CampaignId -> IO (Either T.Text Bool)
validateCampaign pool cmpId = do
  res <- Pool.use pool (validateCampaignSession cmpId)
  return $ over _Left wrapQueryError res
