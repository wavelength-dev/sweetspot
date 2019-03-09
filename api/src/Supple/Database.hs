{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Supple.Database
  ( Connection
  , getDbConnection
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
import qualified Hasql.Session as Session
import Supple.Data.Api
import Supple.Data.Common (EventType(..), Price, Sku, Svid)
import Supple.Data.Common
import Supple.Database.Sessions

type Connection = Connection.Connection

data DbConfig = DbConfig
  { host :: String
  , port :: Int
  , name :: String
  , user :: String
  , password :: String
  }

getDbConnection :: DbConfig -> IO Connection
getDbConnection DbConfig {..} = do
  res <- Connection.acquire connectionSettings
  case res of
    Left err -> error $ maybe "Unknown error during connection aquire" show err
    Right connection -> return connection
  where
    connectionSettings =
      Connection.settings
        (fromString host)
        (fromIntegral port)
        (fromString user)
        (fromString password)
        (fromString name)

wrapQueryError :: Session.QueryError -> T.Text
wrapQueryError = T.pack . show

getUserBuckets :: Connection -> UserId -> IO (Either T.Text [UserBucket])
getUserBuckets conn userId = do
  res <- Session.run (getUserBucketSession userId) conn
  return $ over _Left wrapQueryError res

getNewUserBuckets :: Connection -> IO (Either T.Text [UserBucket])
getNewUserBuckets conn = do
  res <- Session.run assignAndGetUserBucketSession conn
  return $ over _Left wrapQueryError res

getExperimentBuckets :: Connection -> IO (Either T.Text [ExperimentBuckets])
getExperimentBuckets conn = do
  res <- Session.run getBucketsSession conn
  return $ over _Left wrapQueryError res

insertEvent :: Connection -> TrackView -> IO (Either T.Text ())
insertEvent conn tv = do
  res <- Session.run (insertEventSession input) conn
  return $ over _Left wrapQueryError res
  where
    input = (View, toJSON tv)

createExperiment ::
     Connection
  -> Sku
  -> Svid
  -> Price
  -> CampaignId
  -> T.Text
  -> IO (Either T.Text ())
createExperiment conn sku svid price cmp name = do
  res <-
    Session.run (createExperimentSession (sku, svid, price, cmp, name)) conn
  return $ over _Left wrapQueryError res

getExperimentStats :: Connection -> ExpId -> IO (Either T.Text ExperimentStats)
getExperimentStats conn expId = do
  res <- Session.run (getExperimentStatsSession expId) conn
  return $ over _Left wrapQueryError res
