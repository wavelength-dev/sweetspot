{-# LANGUAGE OverloadedStrings #-}

module Supple.Database.Statements.Dashboard where

import Control.Lens ((^..), (&))
import Data.Aeson.Lens (values, key, _Integer)
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement(..))
import Supple.Data.Api (Experiment(..), Bucket(..))
import Supple.Data.Common
import Supple.Data.Domain (CheckoutEvent(..), Campaign(..))

getCampaignStatement :: Statement CampaignId Campaign
getCampaignStatement = Statement sql encoder decoder True
  where
    sql =
      mconcat
        [ "SELECT campaign_id, name, min_profit_increase, start_date, end_date "
        , "FROM campaigns WHERE campaign_id = $1;"
        ]
    encoder = unwrapCampaignId >$< Encoders.param Encoders.text
    decoder = Decoders.singleRow $ toCampaign <$> row
    row =
      (,,,,) <$> Decoders.column Decoders.text <*>
      Decoders.column Decoders.text <*>
      Decoders.column Decoders.int8 <*>
      Decoders.column Decoders.timestamptz <*>
      Decoders.column Decoders.timestamptz
    toCampaign =
      \(cmpId, name, increase, start, end) ->
        Campaign
          { _cCampaignId = (CampaignId cmpId)
          , _cCampaignName = name
          , _cMinProfitIncrease = fromIntegral increase
          , _cStartDate = start
          , _cEndDate = end
          }

getCampaignExperimentsStatement :: Statement CampaignId [Experiment]
getCampaignExperimentsStatement = Statement sql encoder decoder True
  where
    sql =
      mconcat
        [ "SELECT e.exp_id, e.sku, e.product_name FROM experiments AS e "
        , "JOIN campaign_experiments AS ce ON e.exp_id = ce.exp_id "
        , "WHERE ce.campaign_id = $1;"
        ]
    encoder = unwrapCampaignId >$< Encoders.param Encoders.text
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

getBucketUserCountStatement :: Statement BucketId Int
getBucketUserCountStatement = Statement sql encoder decoder True
  where
    sql =
      "SELECT COUNT(DISTINCT user_id) FROM bucket_users WHERE bucket_id = $1;"
    encoder = toDatabaseInt >$< Encoders.param Encoders.int8
    decoder =
      Decoders.singleRow $ fromIntegral <$> Decoders.column Decoders.int8

getBucketImpressionCountStatement :: Statement BucketId Int
getBucketImpressionCountStatement = Statement sql encoder decoder True
  where
    sql =
      mconcat
        [ "SELECT COUNT(*) FROM events AS e "
        , "JOIN bucket_users AS bu ON bu.user_id = (e.payload->>'userId')::int "
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
