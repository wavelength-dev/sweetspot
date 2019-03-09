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
  ) where

import Control.Lens (_Left, over)
import Data.Aeson (toJSON)
import Data.ByteString.UTF8 (fromString)
import qualified Data.Text as T
import qualified Hasql.Connection as Connection
import qualified Hasql.Pool as Pool
import Supple.Data.Api
import Supple.Data.Common (EventType(..), Price, Sku, Svid)
import Supple.Data.Common
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

getNewUserBuckets :: Pool -> IO (Either T.Text [UserBucket])
getNewUserBuckets pool = do
  res <- Pool.use pool assignAndGetUserBucketSession
  return $ over _Left wrapQueryError res

getExperimentBuckets :: Pool -> IO (Either T.Text [ExperimentBuckets])
getExperimentBuckets pool = do
  res <- Pool.use pool getBucketsSession
  return $ over _Left wrapQueryError res

insertEvent :: Pool -> TrackView -> IO (Either T.Text ())
insertEvent pool tv = do
  res <- Pool.use pool (insertEventSession input)
  return $ over _Left wrapQueryError res
  where
    input = (View, toJSON tv)

createExperiment ::
     Pool
  -> Sku
  -> Svid
  -> Price
  -> CampaignId
  -> T.Text
  -> IO (Either T.Text ())
createExperiment pool sku svid price cmp name = do
  res <- Pool.use pool (createExperimentSession (sku, svid, price, cmp, name))
  return $ over _Left wrapQueryError res

getExperimentStats :: Pool -> ExpId -> IO (Either T.Text ExperimentStats)
getExperimentStats pool expId = do
  res <- Pool.use pool (getExperimentStatsSession expId)
  return $ over _Left wrapQueryError res
