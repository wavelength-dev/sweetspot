{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Database.Statements.Dashboard where

import Data.Aeson (fromJSON, Value, Result(..))
import Data.Functor.Contravariant ((>$<))
import Data.Time.LocalTime (utcToLocalTime, utc)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement(..))
import SweetSpot.Data.Api (Experiment(..), Bucket(..))
import SweetSpot.Data.Common
import SweetSpot.Data.Domain

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
          (,,,,,) <$> Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.text <*>
          Decoders.column Decoders.numeric <*>
          Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.numeric
        toBucket =
          \(bid, btype, p, ogSvid, testSvid, original_price) ->
            Bucket
              { _bBucketId = BucketId $ fromIntegral bid
              , _bBucketType = bucketTypeFromText btype
              , _bPrice = Price p
              , _bOriginalSvid = Svid $ fromIntegral ogSvid
              , _bTestSvid = Svid $ fromIntegral testSvid
              , _bControlPrice = Price original_price
              }

parseLineItems :: Value -> [LineItem]
parseLineItems v =
  case fromJSON v of
    Success r -> r
    Error err -> error err
