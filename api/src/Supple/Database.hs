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

import Data.Aeson (toJSON)
import Data.ByteString.UTF8 (fromString)
import Data.Text (Text)
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
import Supple.Data.Common (EventType(..), Price, Sku, Svid)
import Supple.Data.Api
import Supple.Database.Sessions
import Supple.Data.Common

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

getUserBuckets :: Connection -> UserId -> IO [UserBucket]
getUserBuckets conn userId = do
  Right res <- Session.run (getUserBucketSession userId) conn
  return res

getNewUserBuckets :: Connection -> IO [UserBucket]
getNewUserBuckets conn = do
  res <- Session.run assignAndGetUserBucketSession conn
  case res of
    Right res -> return res
    Left err -> do
      print err
      return []

getExperimentBuckets :: Connection -> IO [ExperimentBuckets]
getExperimentBuckets conn = do
  res <- Session.run getBucketsSession conn
  case res of
    Right res -> return res
    Left err -> do
      print err
      return []

insertEvent :: Connection -> TrackView -> IO ()
insertEvent conn tv = do
  res <- Session.run (insertEventSession input) conn
  case res of
    Right _ -> return ()
    Left err -> print err >> return ()
  where
    input = (View, toJSON tv)

createExperiment :: Connection -> Sku -> Svid -> Price -> CampaignId -> Text -> IO ()
createExperiment conn sku svid price cmp name = do
  res <-
    Session.run
      (createExperimentSession (sku, svid, price, cmp, name))
      conn
  case res of
    Right _ -> return ()
    Left err -> print err >> return ()

getExperimentStats :: Connection -> ExpId -> IO ExperimentStats
getExperimentStats conn expId = do
  res <- Session.run (getExperimentStatsSession expId) conn
  case res of
    Right res -> return res
    Left err -> do
      print err
      undefined
