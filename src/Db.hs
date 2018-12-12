{-# LANGUAGE OverloadedStrings #-}

module Db
  ( getDbConnection
  , getUserBucket
  , insertBucket
  , Connection
  ) where

import Data.Text (Text)
import Database.PostgreSQL.Simple
  ( Connection
  , Query
  , connect
  , connectDatabase
  , defaultConnectInfo
  , execute
  , query
  , query_
  )
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import Types

userBucketQuery =
  (mconcat
     [ "SELECT buckets.price, buckets.variant_id FROM user_buckets "
     , "INNER JOIN users ON user_buckets.user_id = users.user_id "
     , "INNER JOIN buckets ON user_buckets.variant_id = buckets.variant_id "
     , "WHERE users.user_id = ? AND buckets.sku = ?;"
     ]) :: Query

insertBucketQuery =
  "INSERT INTO buckets (variant_id, sku, price) VALUES (?, ?, ?);" :: Query

getDbConnection :: IO Connection
getDbConnection = connect defaultConnectInfo {connectDatabase = "supple"}

getUserBucket :: Connection -> Int -> Text -> IO (Maybe UserBucket)
getUserBucket conn uid sku = do
  rows <- query conn userBucketQuery (uid, sku)
  if length rows > 0
    then (return . Just . head) rows
    else return Nothing

insertBucket :: Connection -> Bucket -> IO ()
insertBucket conn bucket = do
  _ <-
    execute conn insertBucketQuery (variant_id bucket, sku bucket, price bucket)
  return ()
