{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Supple.Database
  ( Connection
  , getDbConnection
  , getUserBuckets
  , getNewUserBuckets
  , getExperimentBuckets
  ) where

import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
import Supple.Types
import Supple.Database.Sessions

type Connection = Connection.Connection

getDbConnection :: IO Connection
getDbConnection = do
  Right connection <- Connection.acquire connectionSettings
  return connection
  where
    connectionSettings =
      Connection.settings "localhost" 5432 "postgres" "" "supple"

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
