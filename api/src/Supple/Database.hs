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
  , DbConfig(..)
  , insertEvent
  ) where

import Data.Aeson (toJSON)
import Data.ByteString.UTF8 (fromString)
import Data.Text (Text)
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
import Supple.Data.Api (TrackView)
import Supple.Data.Common (EventType(..), Price, Sku, Svid)
import Supple.Data.Database (ExperimentBuckets, UserBucket)
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
  Right connection <- Connection.acquire connectionSettings
  return connection
  where
    connectionSettings =
      Connection.settings
        (fromString host)
        (fromIntegral port)
        (fromString user)
        (fromString password)
        (fromString name)

getUserBuckets :: Connection -> Int -> IO [UserBucket]
getUserBuckets conn userId = do
  Right res <- Session.run (getUserBucketSession $ fromIntegral userId) conn
  return res

getNewUserBuckets :: Connection -> IO [UserBucket]
getNewUserBuckets conn = do
  res <- Session.run assignAndGetUserBucketSession conn
  case res of
    Right res -> return res
    Left err -> do
      putStrLn $ show err
      return []

getExperimentBuckets :: Connection -> IO [ExperimentBuckets]
getExperimentBuckets conn = do
  res <- Session.run getBucketsSession conn
  case res of
    Right res -> return res
    Left err -> do
      putStrLn $ show err
      return []

insertEvent :: Connection -> TrackView -> IO ()
insertEvent conn tv = do
  res <- Session.run (insertEventSession input) conn
  case res of
    Right _ -> return ()
    Left err -> (putStrLn . show) err *> return ()
  where
    input = (View, toJSON tv)

createExperiment :: Connection -> Sku -> Svid -> Price -> Text -> IO ()
createExperiment conn sku svid price name = do
  res <-
    Session.run
      (createExperimentSession (sku, svid, price, name))
      conn
  case res of
    Right _ -> return ()
    Left err -> (putStrLn . show) err *> return ()
