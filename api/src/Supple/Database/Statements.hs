{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Supple.Database.Statements where

import Data.Aeson (Value)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement(..))
import Supple.Data.Common (EventType, Price(..))
import Supple.Data.Database

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
              , bucket_price = Price price
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
      Decoders.rowList $ (,) <$> Decoders.column Decoders.int8 <*>
      Decoders.column Decoders.int8

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

insertExperimentStatement :: Statement (Text, Text) ExpId
insertExperimentStatement = Statement sql encoder decoder True
  where
    sql =
      mconcat
        [ "INSERT INTO experiments (sku, name) "
        , "VALUES ($1, $2) RETURNING exp_id;"
        ]
    encoder =
      (fst >$< Encoders.param Encoders.text) <>
      (snd >$< Encoders.param Encoders.text)
    decoder = Decoders.singleRow $ Decoders.column Decoders.int8

insertBucketStatement :: Statement (Int64, Text, Price) BucketId
insertBucketStatement = Statement sql encoder decoder True
  where
    sql =
      mconcat
        ["INSERT INTO buckets (svid, sku, price) ", "VALUES ($1, $2, $3);"]
    encoder =
      (fst' >$< Encoders.param Encoders.int8) <>
      (snd' >$< Encoders.param Encoders.text) <>
      (getPrice >$< Encoders.param Encoders.numeric)
    decoder = Decoders.singleRow $ Decoders.column Decoders.int8
    fst' (x, _, _) = x
    snd' (_, x, _) = x
    getPrice (_, _, (Price p)) = p

insertExperimentBucketStatement :: Statement (ExpId, BucketId) ()
insertExperimentBucketStatement = Statement sql encoder Decoders.unit True
  where
    sql =
      mconcat
        [ "INSERT INTO experiment_buckets (exp_id, bucket_id) "
        , "VALUES ($1, $2);"
        ]
    encoder =
      (fst >$< Encoders.param Encoders.int8) <>
      (snd >$< Encoders.param Encoders.int8)

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
              { bucket_id = fromIntegral bid
              , price = Price p
              , svid = fromIntegral sv
              }

insertEventStatement :: Statement (EventType, Value) ()
insertEventStatement = Statement sql encoder decoder True
  where
    sql = "INSERT INTO events (type, payload) VALUES ($1, $2);"
    encoder =
      (eventTypeToText . fst >$< Encoders.param Encoders.text) <>
      (snd >$< Encoders.param Encoders.jsonb)
    decoder = Decoders.unit
