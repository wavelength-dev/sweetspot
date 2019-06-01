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

userBucketStatement :: Statement UserId [UserBucket]
userBucketStatement = Statement sql encoder decoder True
  where
    sql =
      mconcat
        [ "SELECT bu.user_id, e.sku, b.original_svid, b.test_svid, b.price, eb.exp_id, b.bucket_id "
        , "FROM bucket_users AS bu "
        , "JOIN buckets AS b ON bu.bucket_id = b.bucket_id "
        , "JOIN experiment_buckets AS eb ON eb.bucket_id = b.bucket_id "
        , "JOIN experiments AS e ON eb.exp_id = e.exp_id "
        , "JOIN campaign_users AS cu ON bu.user_id = cu.user_id "
        , "JOIN campaigns AS c ON cu.campaign_id = c.campaign_id "
        , "WHERE bu.user_id = $1 and c.start_date < now() and c.end_date > now();"]
    encoder = toDatabaseInt >$< Encoders.param Encoders.int8
    decoder = Decoders.rowList $ toUserBucket <$> row
      where
        row =
          (,,,,,,) <$> Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.text <*>
          Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.numeric <*>
          Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.int8
        toUserBucket =
          \(uid, sku, original_svid, test_svid, price, expId, bucketId) ->
            UserBucket
              { _ubUserId = UserId $ fromIntegral uid
              , _ubSku = Sku sku
              , _ubOriginalSvid = Svid $ fromIntegral original_svid
              , _ubTestSvid = Svid $ fromIntegral test_svid
              , _ubPrice = Price price
              , _ubExpId = ExpId $ fromIntegral expId
              , _ubBucketId = BucketId $ fromIntegral bucketId
              }

insertUserStatement :: Statement () UserId
insertUserStatement = Statement sql Encoders.unit decoder True
  where
    sql = "INSERT INTO users (user_id) VALUES (DEFAULT) RETURNING user_id;"
    decoder = Decoders.singleRow $ wrapUserId <$> Decoders.column Decoders.int8
    wrapUserId userId = UserId $ fromIntegral userId

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
        [ "SELECT exp_id, sku, product_name FROM experiments "
        , "WHERE exp_id = $1;"
        ]
    encoder = toDatabaseInt >$< Encoders.param Encoders.int8
    decoder = Decoders.singleRow $ toExperiment <$> row
      where
        row =
          (,,) <$> Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.text <*>
          Decoders.column Decoders.text
        toExperiment =
          \(expId, sku, pName) ->
            Experiment
              { _eExpId = ExpId $ fromIntegral expId
              , _eSku = Sku sku
              , _eProductName = pName
              }

getExperimentsStatement :: Statement () [Experiment]
getExperimentsStatement = Statement sql Encoders.unit decoder True
  where
    sql = "SELECT exp_id, sku, product_name FROM experiments;"
    decoder = Decoders.rowList $ toExperiment <$> row
      where
        row =
          (,,) <$> Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.text <*>
          Decoders.column Decoders.text
        toExperiment =
          \(expId, sku, pName) ->
            Experiment
              { _eExpId = ExpId $ fromIntegral expId
              , _eSku = Sku sku
              , _eProductName = pName
              }

insertExperimentStatement :: Statement (Sku, Text) ExpId
insertExperimentStatement = Statement sql encoder decoder True
  where
    sql =
      mconcat
        [ "INSERT INTO experiments (sku, product_name) "
        , "VALUES ($1, $2) RETURNING exp_id;"
        ]
    encoder =
      (getSku >$< Encoders.param Encoders.text) <>
      (getName >$< Encoders.param Encoders.text)
    getSku (Sku txt, _) = txt
    getName (_, name) = name
    decoder = Decoders.singleRow $ wrapExpId <$> Decoders.column Decoders.int8
    wrapExpId eid = ExpId $ fromIntegral eid

insertBucketStatement :: Statement (BucketType, Svid, Svid, Sku, Price) BucketId
insertBucketStatement = Statement sql encoder decoder True
  where
    sql =
      mconcat
        [ "INSERT INTO buckets (bucket_type, original_svid, test_svid, sku, price) "
        , "VALUES ($1, $2, $3) RETURNING bucket_id;"
        ]
    encoder =
      (getBucketType >$< Encoders.param Encoders.text) <>
      (getOrigSvid >$< Encoders.param Encoders.int8) <>
      (getTestSvid >$< Encoders.param Encoders.int8) <>
      (getSku >$< Encoders.param Encoders.text) <>
      (getPrice >$< Encoders.param Encoders.numeric)
    decoder = Decoders.singleRow $ wrapBuid <$> Decoders.column Decoders.int8
    wrapBuid buid = BucketId $ fromIntegral buid
    getBucketType (t, _, _, _, _) = bucketTypeToText t
    getOrigSvid (_, s, _, _, _) = toDatabaseInt s
    getTestSvid (_, _, s, _, _) = toDatabaseInt s
    getSku (_, _, _, (Sku s), _) = s
    getPrice (_, _, _, _, (Price p)) = p

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
        [ "SELECT bs.bucket_id, bs.bucket_type, bs.price, bs.original_svid, bs.test_svid "
        , "FROM experiment_buckets as ebs "
        , "INNER JOIN buckets as bs ON bs.bucket_id = ebs.bucket_id "
        , "WHERE ebs.exp_id = $1;"
        ]
    encoder = toDatabaseInt >$< Encoders.param Encoders.int8
    decoder = Decoders.rowList $ toBucket <$> row
      where
        row =
          (,,,,) <$> Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.text <*>
          Decoders.column Decoders.numeric <*>
          Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.int8
        toBucket =
          \(bid, btype, p, ogSvid, testSvid) ->
            Bucket
              { _bBucketId = BucketId $ fromIntegral bid
              , _bBucketType = bucketTypeFromText btype
              , _bPrice = Price p
              , _bOriginalSvid = Svid $ fromIntegral ogSvid
              , _bTestSvid = Svid $ fromIntegral testSvid
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
      \(chkId, ts, userId, bucketId, orderId, lineItems) ->
        CheckoutEvent
          { _chkId = EventId $ fromIntegral chkId
          , _chkTimestamp = ts
          , _chkUserId = UserId $ fromIntegral userId
          , _chkBucketId = BucketId $ fromIntegral bucketId
          , _chkOrderId = OrderId $ fromIntegral orderId
          , _chkLineItems =
              lineItems ^.. values . key "variantId" . _Integer
                & fmap (Svid . fromIntegral)
          }

getActiveCampaignById :: Statement CampaignId (Maybe CampaignId)
getActiveCampaignById = Statement sql encoder decoder True
  where
    sql =
      mconcat
        [ "SELECT campaign_id FROM campaigns AS c "
        , "WHERE c.campaign_id = $1 AND c.start_date < now() AND c.end_date > now();"]
    encoder = getCmpId >$< Encoders.param Encoders.text
      where getCmpId (CampaignId id) = id
    decoder = Decoders.rowMaybe $ wrapCmpId <$> (Decoders.column Decoders.text)
    wrapCmpId cid = CampaignId $ cid
