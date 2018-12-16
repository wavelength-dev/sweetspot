{-# LANGUAGE OverloadedStrings #-}

module Database
  ( Connection
  , getDbConnection
  , getUserBuckets
  , getNewUserBuckets
  ) where

import Control.Monad (forM_)
import Data.Functor.Contravariant ((>$<))
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

--
-- Statements
--
userBucketsStatement :: Statement Int64 [UserBucket]
userBucketsStatement = Statement sql encoder decoder True
  where
    sql =
      (mconcat
         [ "SELECT users.user_id, buckets.sku, buckets.svid, buckets.price FROM bucket_users "
         , "INNER JOIN users ON bucket_users.user_id = users.user_id "
         , "INNER JOIN buckets ON bucket_users.bucket_id = buckets.bucket_id "
         , "WHERE users.user_id = $1;"
         ])
    encoder = Encoders.param Encoders.int8
    decoder = Decoders.rowList $ toUserBucket <$> row
      where
        row =
          (,,,) <$> Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.text <*>
          Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.int8
        toUserBucket =
          \(uid, sku, svid, price) ->
            UserBucket
              { user_id = fromIntegral uid
              , bucket_sku = sku
              , bucket_svid = fromIntegral svid
              , bucket_price = fromIntegral price
              }

insertUserStatement :: Statement () Int64
insertUserStatement = Statement sql Encoders.unit decoder True
  where
    sql = "INSERT INTO users (user_id) VALUES (DEFAULT) RETURNING user_id;"
    decoder = Decoders.singleRow $ Decoders.column Decoders.int8

type UserId = Int64

type ExpId = Int64

type BucketId = Int64

randomBucketPerExpStatement :: Statement () [(ExpId, BucketId)]
randomBucketPerExpStatement = Statement sql Encoders.unit decoder True
  where
    sql =
      (mconcat
         [ "SELECT DISTINCT ON (exp_id) exp_id, bucket_id "
         , "FROM (SELECT exp_id, bucket_id FROM experiment_buckets "
         , "ORDER BY random()) as \"subq\";"
         ])
    decoder =
      -- TODO Use vectors instead of lists
      Decoders.rowList $
      (,) <$> Decoders.column Decoders.int8 <*> Decoders.column Decoders.int8

assignUserToBucketStatement :: Statement (UserId, BucketId) ()
assignUserToBucketStatement = Statement sql encoder Decoders.unit True
  where
    sql = "INSERT INTO bucket_users (user_id, bucket_id) VALUES ($1, $2);"
    encoder =
      (fst >$< Encoders.param Encoders.int8) <>
      (snd >$< Encoders.param Encoders.int8)

--
-- Sessions
--
getUserBucketSession :: Int64 -> Session [UserBucket]
getUserBucketSession userId = Session.statement userId userBucketsStatement

assignAndGetUserBucketSession :: Session [UserBucket]
assignAndGetUserBucketSession = do
  uid <- Session.statement () insertUserStatement
  randBs <- Session.statement () randomBucketPerExpStatement
  forM_
    randBs
    (\(_, bucketId) ->
       Session.statement (uid, bucketId) assignUserToBucketStatement)
  Session.statement uid userBucketsStatement

--
-- Exposed functions
--
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
