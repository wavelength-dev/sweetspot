{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Supple.Database
  ( Pool
  , getDbPool
  , getUserBucket
  , createExperiment
  , getNewUserBucket
  , getExperimentBuckets
  , getExperimentStats
  , DbConfig(..)
  , insertEvent
  , insertLogEvent
  ) where

import Control.Lens (_Left, over, (^?))
import Data.Aeson (Value)
import Data.Aeson.Lens (key, _String)
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

getUserBucket :: Pool -> UserId -> IO (Either T.Text UserBucket)
getUserBucket pool userId = do
  res <- Pool.use pool (getUserBucketSession userId)
  return $ over _Left wrapQueryError res

getNewUserBucket :: Pool -> IO (Either T.Text UserBucket)
getNewUserBucket pool = do
  res <- Pool.use pool assignAndGetUserBucketSession
  return $ over _Left wrapQueryError res

getExperimentBuckets :: Pool -> IO (Either T.Text [ExperimentBuckets])
getExperimentBuckets pool = do
  res <- Pool.use pool getBucketsSession
  return $ over _Left wrapQueryError res

insertEvent :: Pool -> Value -> IO (Either T.Text ())
insertEvent pool val = do
  res <- Pool.use pool (insertEventSession input)
  return $ over _Left wrapQueryError res
  where
    pageType = val ^? key "page" . _String
    step = val ^? key "step" . _String
      -- Relies on show instance of page in injectable
    input =
      case (pageType, step) of
        (Just "checkout", Just "thank_you") -> (Checkout, val)
        _ -> (View, val)

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
