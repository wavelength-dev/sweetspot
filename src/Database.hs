{-# LANGUAGE OverloadedStrings #-}

module Database
  ( Connection
  , getDbConnection
  , insertBucket
  , getUserBucket
  ) where

import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Hasql.Connection as Connection
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Session (Session)
import qualified Hasql.Session as Session
import Hasql.Statement (Statement(..))
import Types

type Connection = Connection.Connection

getDbConnection :: IO Connection
getDbConnection = do
  Right connection <- Connection.acquire connectionSettings
  return connection
  where
    connectionSettings =
      Connection.settings "localhost" 5432 "postgres" "" "supple"

userBucketStatement :: Statement (Int64, Text) UserBucket
userBucketStatement = Statement sql encoder decoder True
  where
    sql =
      (mconcat
         [ "SELECT buckets.price, buckets.variant_id FROM user_buckets "
         , "INNER JOIN users ON user_buckets.user_id = users.user_id "
         , "INNER JOIN buckets ON user_buckets.variant_id = buckets.variant_id "
         , "WHERE users.user_id = $1 AND buckets.sku = $2;"
         ])
    encoder =
      (fst >$< Encoders.param Encoders.int8) <>
      (snd >$< Encoders.param Encoders.text)
    decoder = Decoders.singleRow $ toUserBucket <$> row
      where
        row =
          (,) <$> Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.int8
        toUserBucket =
          \(price, variant) ->
            UserBucket
              { bucket_price = fromIntegral price
              , bucket_variant = fromIntegral variant
              }

insertBucketStatement :: Statement Bucket ()
insertBucketStatement = Statement sql encoder decoder True
  where
    sql = "INSERT INTO buckets (variant_id, sku, price) VALUES ($1, $2, $3);"
    encoder =
      (fromIntegral . variant_id >$< Encoders.param Encoders.int8) <>
      (sku >$< Encoders.param Encoders.text) <>
      (fromIntegral . price >$< Encoders.param Encoders.int8)
    decoder = Decoders.unit

getUserBucketSession :: Int64 -> Text -> Session UserBucket
getUserBucketSession userId sku =
  Session.statement (userId, sku) userBucketStatement

insertBucketSession :: Bucket -> Session ()
insertBucketSession bucket = Session.statement bucket insertBucketStatement

getUserBucket :: Connection -> Int -> Text -> IO UserBucket
getUserBucket conn userId sku = do
  Right res <- Session.run (getUserBucketSession (fromIntegral userId) sku) conn
  return res

insertBucket :: Connection -> Bucket -> IO ()
insertBucket conn bucket = do
  Right res <- Session.run (insertBucketSession bucket) conn
  return res
