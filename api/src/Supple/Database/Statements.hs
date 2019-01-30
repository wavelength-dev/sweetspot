{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Supple.Database.Statements where

import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement(..))
import Supple.Types

type UserId = Int64

type ExpId = Int64

type BucketId = Int64

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
          Decoders.column Decoders.numeric
        toUserBucket =
          \(uid, sku, svid, price) ->
            UserBucket
              { user_id = fromIntegral uid
              , bucket_sku = sku
              , bucket_svid = fromIntegral svid
              , bucket_price = price
              }

insertUserStatement :: Statement () Int64
insertUserStatement = Statement sql Encoders.unit decoder True
  where
    sql = "INSERT INTO users (user_id) VALUES (DEFAULT) RETURNING user_id;"
    decoder = Decoders.singleRow $ Decoders.column Decoders.int8

randomBucketPerExpStatement :: Statement () [(ExpId, BucketId)]
randomBucketPerExpStatement = Statement sql Encoders.unit decoder True
  where
    sql =
      (mconcat
         [ "SELECT DISTINCT ON (exp_id) exp_id, bucket_id "
         , "FROM (SELECT exp_id, bucket_id FROM experiment_buckets "
         , "ORDER BY random()) as \"subq\";"
         ])
    decoder
      -- TODO Use vectors instead of lists
     =
      Decoders.rowList $
      (,) <$> Decoders.column Decoders.int8 <*> Decoders.column Decoders.int8

assignUserToBucketStatement :: Statement (UserId, BucketId) ()
assignUserToBucketStatement = Statement sql encoder Decoders.unit True
  where
    sql = "INSERT INTO bucket_users (user_id, bucket_id) VALUES ($1, $2);"
    encoder =
      (fst >$< Encoders.param Encoders.int8) <>
      (snd >$< Encoders.param Encoders.int8)

getExperimentsStatement :: Statement () [Experiment]
getExperimentsStatement = Statement sql Encoders.unit decoder True
  where
    sql = "SELECT exp_id, sku, name FROM experiments;"
    decoder = Decoders.rowList $ toExperiment <$> row
      where
        row =
          (,,) <$> Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.text <*>
          Decoders.column Decoders.text
        toExperiment =
          \(exp_id, sku, name) ->
            Experiment {exp_id = fromIntegral exp_id, sku = sku, name = name}

getBucketsForExperimentStatement :: Statement ExpId [Bucket]
getBucketsForExperimentStatement = Statement sql encoder decoder True
  where
    sql =
      mconcat
        [ "SELECT bs.bucket_id, bs.price, bs.svid "
        , "FROM experiment_buckets as ebs "
        , "INNER JOIN buckets as bs ON bs.bucket_id = ebs.bucket_id "
        , "WHERE ebs.exp_id = $1;"
        ]
    encoder = Encoders.param Encoders.int8
    decoder = Decoders.rowList $ toBucket <$> row
      where
        row =
          (,,) <$> Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.numeric <*>
          Decoders.column Decoders.int8
        toBucket =
          \(bid, p, sv) ->
            Bucket
              {bucket_id = fromIntegral bid, price = p, svid = fromIntegral sv}
