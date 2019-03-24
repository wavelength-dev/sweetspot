{-# LANGUAGE OverloadedStrings #-}

module Supple.Database.Statements where

import Control.Lens ((&), (^..))
import Data.Aeson (Value)
import Data.Aeson.Lens (_Integer, key, values)
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement(..))
import Supple.Data.Api
import Supple.Data.Common
import Supple.Data.Domain (CheckoutEvent(..))

userBucketStatement :: Statement UserId UserBucket
userBucketStatement = Statement sql encoder decoder True
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
    encoder = toDatabaseInt >$< Encoders.param Encoders.int8
    decoder = Decoders.singleRow $ toUserBucket <$> row
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
      (toDatabaseInt . fst >$< Encoders.param Encoders.int8) <>
      (toDatabaseInt . snd >$< Encoders.param Encoders.int8)

getExperimentStatement :: Statement ExpId Experiment
getExperimentStatement = Statement sql encoder decoder True
  where
    sql =
      mconcat
        [ "SELECT exp_id, sku, name, campaign_id FROM experiments "
        , "WHERE exp_id = $1;"
        ]
    encoder = toDatabaseInt >$< Encoders.param Encoders.int8
    decoder = Decoders.singleRow $ toExperiment <$> row
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
              , _eCampaignId = CampaignId cmpId
              }

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
              , _eCampaignId = CampaignId cmpId
              }

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
    getSvid (s, _, _) = toDatabaseInt s
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
      (toDatabaseInt . fst >$< Encoders.param Encoders.int8) <>
      (toDatabaseInt . snd >$< Encoders.param Encoders.int8)

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
    encoder = toDatabaseInt >$< Encoders.param Encoders.int8
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

insertEventStatement :: Statement (EventType, Value) ()
insertEventStatement = Statement sql encoder decoder True
  where
    sql = "INSERT INTO events (type, payload) VALUES ($1, $2);"
    encoder =
      (eventTypeToText . fst >$< Encoders.param Encoders.text) <>
      (snd >$< Encoders.param Encoders.jsonb)
    decoder = Decoders.unit

getBucketUserCountStatement :: Statement BucketId Int
getBucketUserCountStatement = Statement sql encoder decoder True
  where
    sql =
      "SELECT COUNT(DISTINCT user_id) FROM bucket_users WHERE bucket_id = $1;"
    encoder = toDatabaseInt >$< Encoders.param Encoders.int8
    decoder =
      Decoders.singleRow $ fromIntegral <$> Decoders.column Decoders.int8

-- | The below two queries assume there will only exist one bucket per user
-- | at a time.
getBucketImpressionCountStatement :: Statement BucketId Int
getBucketImpressionCountStatement = Statement sql encoder decoder True
  where
    sql =
      mconcat
        [ "SELECT COUNT(*) FROM events AS e INNER JOIN bucket_users as bu "
        , "ON bu.user_id = (e.payload->>'userId')::int "
        , "WHERE type = 'view' AND bu.bucket_id = $1;"
        ]
    encoder = toDatabaseInt >$< Encoders.param Encoders.int8
    decoder =
      Decoders.singleRow $ fromIntegral <$> Decoders.column Decoders.int8

getCheckoutEventsForBucket :: Statement BucketId [CheckoutEvent]
getCheckoutEventsForBucket = Statement sql encoder decoder True
  where
    sql =
      mconcat
        [ "SELECT e.id, e.timestamp, bu.user_id, bu.bucket_id, e.payload->>'orderId', "
        , "e.payload->>'lineItems' "
        , "FROM events AS e INNER JOIN bucket_users AS bu "
        , "ON bu.user_id = (e.payload->>'userId')::int "
        , "WHERE type = 'checkout' AND bu.bucket_id = $1;"
        ]
    encoder = toDatabaseInt >$< Encoders.param Encoders.int8
    decoder = Decoders.rowList $ toCheckoutEvent <$> row
    row =
      (,,,,,) <$> Decoders.column Decoders.int8 <*>
      Decoders.column Decoders.timestamptz <*>
      Decoders.column Decoders.int8 <*>
      Decoders.column Decoders.int8 <*>
      Decoders.column Decoders.int8 <*>
      Decoders.column Decoders.json
    toCheckoutEvent =
      \(id, ts, userId, bucketId, orderId, lineItems) ->
        CheckoutEvent
          { _chkId = EventId $ fromIntegral id
          , _chkTimestamp = ts
          , _chkUserId = UserId $ fromIntegral userId
          , _chkBucketId = BucketId $ fromIntegral bucketId
          , _chkOrderId = OrderId $ fromIntegral orderId
          , _chkLineItems =
              lineItems ^.. values . key "variantId" . _Integer
                & fmap (Svid . fromIntegral)
          }
