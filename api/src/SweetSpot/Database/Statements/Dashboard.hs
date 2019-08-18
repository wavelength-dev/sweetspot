{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Database.Statements.Dashboard where

import Data.Aeson (fromJSON, Value, Result(..))
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement(..))
import SweetSpot.Data.Api (Experiment(..), Bucket(..))
import SweetSpot.Data.Common
import SweetSpot.Data.Domain

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
      Decoders.nullableColumn Decoders.timestamptz <*>
      Decoders.nullableColumn Decoders.timestamptz
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

-- | ---------------------------------------------------------------------------
-- | Stats
-- | ---------------------------------------------------------------------------

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
        , "WHERE type = 'view' AND (e.payload->>'userId')::int IN "
        , "(SELECT DISTINCT(bu.user_id) FROM bucket_users bu WHERE bu.bucket_id = $1);"
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
          , _chkLineItems = parseLineItems lineItems
          }

parseLineItems :: Value -> [LineItem]
parseLineItems v =
  case fromJSON v of
    Success r -> r
    Error err -> error err
