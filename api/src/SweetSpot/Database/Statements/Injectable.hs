{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Database.Statements.Injectable where

import Data.Aeson (Value)
import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement(..))
import SweetSpot.Data.Api (UserBucket(..))
import SweetSpot.Data.Common

userBucketStatement :: Statement UserId [UserBucket]
userBucketStatement = Statement sql encoder decoder True
  where
    sql =
      mconcat
        [ "SELECT bu.user_id, e.sku, b.original_svid, b.test_svid, b.price, eb.exp_id, b.bucket_id, b.bucket_type "
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
          (,,,,,,,) <$> Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.text <*>
          Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.numeric <*>
          Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.int8 <*>
          Decoders.column Decoders.text
        toUserBucket =
          \(uid, sku, original_svid, test_svid, price, expId, bucketId, bucketType) ->
            UserBucket
              { _ubUserId = UserId $ fromIntegral uid
              , _ubSku = Sku sku
              , _ubOriginalSvid = Svid $ fromIntegral original_svid
              , _ubTestSvid = Svid $ fromIntegral test_svid
              , _ubPrice = Price price
              , _ubExpId = ExpId $ fromIntegral expId
              , _ubBucketId = BucketId $ fromIntegral bucketId
              , _ubBucketType = bucketTypeFromText bucketType
              }

insertUserStatement :: Statement () UserId
insertUserStatement = Statement sql Encoders.unit decoder True
  where
    sql = "INSERT INTO users (user_id) VALUES (DEFAULT) RETURNING user_id;"
    decoder = Decoders.singleRow $ wrapUserId <$> Decoders.column Decoders.int8

bucketByTypePerExpInCampaignStatement :: Statement (CampaignId, BucketType) [(ExpId, BucketId)]
bucketByTypePerExpInCampaignStatement = Statement sql encoder decoder True
  where
    sql =
      mconcat
         [ "SELECT eb.exp_id, eb.bucket_id FROM experiment_buckets AS eb "
         , "JOIN campaign_experiments AS ce ON eb.exp_id = ce.exp_id "
         , "JOIN buckets AS b ON eb.bucket_id = b.bucket_id "
         , "WHERE ce.campaign_id = $1 and b.bucket_type = $2;"
         ]
    encoder =
      (unwrapCampaignId . fst >$< Encoders.param Encoders.text) <>
      (bucketTypeToText . snd >$< Encoders.param Encoders.text)
    decoder = Decoders.rowList $ wrapRow <$> row
    row =
      (,) <$> (Decoders.column Decoders.int8) <*> Decoders.column Decoders.int8
    wrapRow (eid, bid) = (ExpId $ fromIntegral eid, BucketId $ fromIntegral bid)


assignUserToCampaignStatement :: Statement (UserId, CampaignId) CampaignId
assignUserToCampaignStatement = Statement sql encoder decoder True
  where
    sql = "INSERT INTO campaign_users (user_id, campaign_id) VALUES ($1, $2) RETURNING campaign_id;"
    encoder =
      (toDatabaseInt . fst >$< Encoders.param Encoders.int8) <>
      (unwrapCampaignId . snd >$< Encoders.param Encoders.text)
    decoder = Decoders.singleRow $ wrapCampaignId <$> Decoders.column Decoders.text

assignUserToBucketStatement :: Statement (UserId, BucketId) ()
assignUserToBucketStatement = Statement sql encoder Decoders.unit True
  where
    sql = "INSERT INTO bucket_users (user_id, bucket_id) VALUES ($1, $2);"
    encoder =
      (toDatabaseInt . fst >$< Encoders.param Encoders.int8) <>
      (toDatabaseInt . snd >$< Encoders.param Encoders.int8)

getActiveCampaignById :: Statement CampaignId (Maybe CampaignId)
getActiveCampaignById = Statement sql encoder decoder True
  where
    sql =
      mconcat
        [ "SELECT campaign_id FROM campaigns AS c "
        , "WHERE c.campaign_id = $1 AND c.start_date < now() AND c.end_date > now();"]
    encoder = unwrapCampaignId >$< Encoders.param Encoders.text
    decoder = Decoders.rowMaybe $ wrapCampaignId <$> (Decoders.column Decoders.text)

insertEventStatement :: Statement (EventType, Value) ()
insertEventStatement = Statement sql encoder decoder True
  where
    sql = "INSERT INTO events (type, payload) VALUES ($1, $2);"
    encoder =
      (eventTypeToText . fst >$< Encoders.param Encoders.text) <>
      (snd >$< Encoders.param Encoders.jsonb)
    decoder = Decoders.unit
