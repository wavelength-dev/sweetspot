{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Supple.Database.Statements where

import Data.Aeson (Value)
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement(..))
import Supple.Data.Common
import Supple.Data.Api

userBucketsStatement :: Statement UserId [UserBucket]
userBucketsStatement = Statement sql encoder decoder True
  where
    sql =
      (mconcat
         [ "SELECT users.user_id, buckets.sku, buckets.svid, buckets.price, "
         , "experiment_buckets.exp_id, buckets.bucket_id "
         , "FROM bucket_users "
         , "INNER JOIN users ON bucket_users.user_id = users.user_id "
         , "INNER JOIN buckets ON bucket_users.bucket_id = buckets.bucket_id "
         , "INNER JOIN experiment_buckets ON experiment_buckets.bucket_id = buckets.bucket_id "
         , "WHERE users.user_id = $1;"
         ])
    encoder = unwrapUserId >$< Encoders.param Encoders.int8
    decoder = Decoders.rowList $ toUserBucket <$> row
      where
        row =
          (,,,,,) <$> Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.text <*>
          Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.numeric <*>
          Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.int8
        toUserBucket =
          \(uid, sku, svid, price, expId, bucketId) ->
            UserBucket
              { _ubUserId = UserId $ fromIntegral uid
              , _ubSku = Sku sku
              , _ubSvid = Svid $ fromIntegral svid
              , _ubPrice = Price price
              , _ubExpId = ExpId $ fromIntegral expId
              , _ubBucketId = BucketId $ fromIntegral bucketId
              }
    unwrapUserId (UserId uid) = fromIntegral uid

insertUserStatement :: Statement () UserId
insertUserStatement = Statement sql Encoders.unit decoder True
  where
    sql = "INSERT INTO users (user_id) VALUES (DEFAULT) RETURNING user_id;"
    decoder = Decoders.singleRow $ wrapUserId <$> Decoders.column Decoders.int8
    wrapUserId id = UserId $ fromIntegral id

randomBucketPerExpStatement :: Statement () [(ExpId, BucketId)]
randomBucketPerExpStatement = Statement sql Encoders.unit decoder True
  where
    sql =
      (mconcat
         [ "SELECT DISTINCT ON (exp_id) exp_id, bucket_id "
         , "FROM (SELECT exp_id, bucket_id FROM experiment_buckets "
         , "ORDER BY random()) as \"subq\";"
         ])
    decoder = Decoders.rowList $ wrapRow <$> row
    row =
      (,) <$> (Decoders.column Decoders.int8) <*> Decoders.column Decoders.int8
    wrapRow (eid, bid) = (ExpId $ fromIntegral eid, BucketId $ fromIntegral bid)

assignUserToBucketStatement :: Statement (UserId, BucketId) ()
assignUserToBucketStatement = Statement sql encoder Decoders.unit True
  where
    sql = "INSERT INTO bucket_users (user_id, bucket_id) VALUES ($1, $2);"
    encoder =
      (getUid . fst >$< Encoders.param Encoders.int8) <>
      (getBuid . snd >$< Encoders.param Encoders.int8)
    getUid (UserId id) = fromIntegral id
    getBuid (BucketId id) = fromIntegral id

getExperimentsStatement :: Statement () [Experiment]
getExperimentsStatement = Statement sql Encoders.unit decoder True
  where
    sql = "SELECT exp_id, sku, name, campaign_id FROM experiments;"
    decoder = Decoders.rowList $ toExperiment <$> row
      where
        row =
          (,,,) <$> Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.text <*>
          Decoders.column Decoders.text <*>
          Decoders.column Decoders.text
        toExperiment =
          \(expId, sku, name, cmpId) ->
            Experiment
              { _eExpId = ExpId $ fromIntegral expId
              , _eSku = Sku sku
              , _eName = name
              , _eCampaignId = CampaignId cmpId}

insertExperimentStatement :: Statement (Sku, CampaignId, Text) ExpId
insertExperimentStatement = Statement sql encoder decoder True
  where
    sql =
      mconcat
        [ "INSERT INTO experiments (sku, campaign_id, name) "
        , "VALUES ($1, $2, $3) RETURNING exp_id;"
        ]
    encoder =
      (getSku >$< Encoders.param Encoders.text) <>
      (getCmp >$< Encoders.param Encoders.text) <>
      (getName >$< Encoders.param Encoders.text)
    getSku (Sku txt, _, _) = txt
    getCmp (_, CampaignId id, _) = id
    getName (_, _, name) = name
    decoder = Decoders.singleRow $ wrapExpId <$> Decoders.column Decoders.int8
    wrapExpId id = ExpId $ fromIntegral id

insertBucketStatement :: Statement (Svid, Sku, Price) BucketId
insertBucketStatement = Statement sql encoder decoder True
  where
    sql =
      mconcat
        [ "INSERT INTO buckets (svid, sku, price) "
        , "VALUES ($1, $2, $3) RETURNING bucket_id;"
        ]
    encoder =
      (getSvid >$< Encoders.param Encoders.int8) <>
      (getSku >$< Encoders.param Encoders.text) <>
      (getPrice >$< Encoders.param Encoders.numeric)
    decoder = Decoders.singleRow $ wrapBuid <$> Decoders.column Decoders.int8
    wrapBuid buid = BucketId $ fromIntegral buid
    getSvid (Svid s, _, _) = fromIntegral s
    getSku (_, Sku s, _) = s
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
      (getExpId . fst >$< Encoders.param Encoders.int8) <>
      (getBuId . snd >$< Encoders.param Encoders.int8)
    getExpId (ExpId id) = fromIntegral id
    getBuId (BucketId id) = fromIntegral id

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
    encoder = unwrapExpId >$< Encoders.param Encoders.int8
    decoder = Decoders.rowList $ toBucket <$> row
      where
        row =
          (,,) <$> Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.numeric <*>
          Decoders.column Decoders.int8
        toBucket =
          \(bid, p, sv) ->
            Bucket
              { _bBucketId = BucketId $ fromIntegral bid
              , _bPrice = Price p
              , _bSvid = Svid $ fromIntegral sv
              }
    unwrapExpId (ExpId id) = fromIntegral id

insertEventStatement :: Statement (EventType, Value) ()
insertEventStatement = Statement sql encoder decoder True
  where
    sql = "INSERT INTO events (type, payload) VALUES ($1, $2);"
    encoder =
      (eventTypeToText . fst >$< Encoders.param Encoders.text) <>
      (snd >$< Encoders.param Encoders.jsonb)
    decoder = Decoders.unit
