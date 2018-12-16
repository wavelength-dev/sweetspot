{-# LANGUAGE OverloadedStrings #-}

module Database
  ( Connection
  , getDbConnection
  , getUserBucket
  ) where

import Data.Int (Int64)
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

userBucketStatement :: Statement Int64 [UserBucket]
userBucketStatement = Statement sql encoder decoder True
  where
    sql =
      (mconcat
         [ "SELECT buckets.sku, buckets.svid, buckets.price FROM bucket_users "
         , "INNER JOIN users ON bucket_users.user_id = users.user_id "
         , "INNER JOIN buckets ON bucket_users.bucket_id = buckets.bucket_id "
         , "WHERE users.user_id = $1;"
         ])
    encoder = Encoders.param Encoders.int8
    decoder = Decoders.rowList $ toUserBucket <$> row
      where
        row =
          (,,) <$> Decoders.column Decoders.text <*>
          Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.int8
        toUserBucket =
          \(sku, svid, price) ->
            UserBucket
              { bucket_sku = sku
              , bucket_svid = fromIntegral svid
              , bucket_price = fromIntegral price
              }

getUserBucketSession :: Int64 -> Session [UserBucket]
getUserBucketSession userId = Session.statement userId userBucketStatement

getUserBucket :: Connection -> Int -> IO [UserBucket]
getUserBucket conn userId = do
  Right res <- Session.run (getUserBucketSession $ fromIntegral userId) conn
  return res
