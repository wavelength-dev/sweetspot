{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Supple.Database
  ( Pool
  , getDbPool
  , getUserBuckets
  , createExperiment
  , getNewUserBuckets
  , getExperimentBuckets
  , getExperimentStats
  , DbConfig(..)
  , insertEvent
  , insertLogEvent
  , migrate
  , validateCampaign
  ) where

import Control.Lens (_Left, over)
import Control.Monad (mapM)
import Data.Aeson (Value)
import Data.ByteString.UTF8 (fromString)
import qualified Data.Text as T
import qualified Hasql.Connection as Connection
import Hasql.Migration
  ( MigrationCommand(..)
  , loadMigrationsFromDirectory
  , runMigration
  )
import qualified Hasql.Pool as Pool
import Hasql.Transaction.Sessions (IsolationLevel(..), Mode(..), transaction)
import Supple.Data.Api
import Supple.Data.Common (EventType(..), Price, Sku, Svid)
import Supple.Data.Common
import Supple.Data.Domain (DBExperimentStats)
import Supple.Database.Sessions

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
      Connection.settings
        (fromString host)
        (fromIntegral port)
        (fromString user)
        (fromString password)
        (fromString name)

wrapQueryError :: Pool.UsageError -> T.Text
wrapQueryError = T.pack . show

getUserBuckets :: Pool -> UserId -> IO (Either T.Text [UserBucket])
getUserBuckets pool userId = do
  res <- Pool.use pool (getUserBucketSession userId)
  return $ over _Left wrapQueryError res

getNewUserBuckets :: Pool -> CampaignId -> IO (Either T.Text [UserBucket])
getNewUserBuckets pool cmpId = do
  res <- Pool.use pool (assignAndGetUserBucketSession cmpId)
  return $ over _Left wrapQueryError res

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
  -> CampaignId
  -> T.Text
  -> IO (Either T.Text ())
createExperiment pool sku orig_svid test_svid price cmp name = do
  res <- Pool.use pool (createExperimentSession (sku, orig_svid, test_svid, price, cmp, name))
  return $ over _Left wrapQueryError res

getExperimentStats :: Pool -> ExpId -> IO (Either T.Text DBExperimentStats)
getExperimentStats pool expId = do
  res <- Pool.use pool (getExperimentStatsSession expId)
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
