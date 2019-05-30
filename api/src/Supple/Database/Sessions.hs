{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Supple.Database.Sessions where

import Control.Monad (forM, forM_)
import Data.Aeson (Value)
import Data.Text (Text)
import Hasql.Session (Session)
import qualified Hasql.Session as Session
import Supple.Data.Common
import Supple.Database.Statements
import Supple.Data.Domain
import Supple.Data.Api
import Control.Lens ((^.))

getUserBucketSession :: UserId -> Session UserBucket
getUserBucketSession userId = Session.statement userId userBucketStatement

assignAndGetUserBucketSession :: Session UserBucket
assignAndGetUserBucketSession = do
  uid <- Session.statement () insertUserStatement
  randBs <- Session.statement () randomBucketPerExpStatement
  forM_
    randBs
    (\(_, bucketId) ->
       Session.statement (uid, bucketId) assignUserToBucketStatement)
  Session.statement uid userBucketStatement

getBucketsSession :: Session [ExperimentBuckets]
getBucketsSession = do
  experiments <- Session.statement () getExperimentsStatement
  expBuckets <- forM experiments addBuckets
  return expBuckets
  where
    addBuckets =
      \experiment -> do
        let experimentId = experiment ^. eExpId
        bs <- Session.statement experimentId getBucketsForExperimentStatement
        return
          ExperimentBuckets
            { _ebExpId = experimentId
            , _ebBuckets = bs
            , _ebCampaignId = experiment ^. eCampaignId
            , _ebMinProfitIncrease = experiment ^. eMinProfitIncrease
            , _ebName = experiment ^. eName
            , _ebSku = experiment ^. eSku
            }

insertEventSession :: (EventType, Value) -> Session ()
insertEventSession input = Session.statement input insertEventStatement

createExperimentSession :: (Sku, Svid, Price, CampaignId, Text) -> Session ()
createExperimentSession (sku, svid, price, cmp, name) = do
  expId <- Session.statement (sku, cmp, name) insertExperimentStatement
  -- TODO: create both test and control here
  bucketId <- Session.statement (Test, svid, sku, price) insertBucketStatement
  Session.statement (expId, bucketId) insertExperimentBucketStatement

getExperimentStatsSession :: ExpId -> Session DBExperimentStats
getExperimentStatsSession expId = do
  experiment <- Session.statement expId getExperimentStatement
  bs <- Session.statement expId getBucketsForExperimentStatement
  bstats <- mapM getBucketStats bs
  return $ DBExperimentStats
    { _desExpId = experiment ^. eExpId
    , _desMinProfitIncrease = experiment ^. eMinProfitIncrease
    , _desBuckets = bstats
    }

  where
    getBucketStats :: Bucket -> Session DBBucketStats
    getBucketStats b = do
      let
        bucketId = b ^. bBucketId
        svid = b ^. bSvid
      users <- Session.statement bucketId getBucketUserCountStatement
      impressions <- Session.statement bucketId getBucketImpressionCountStatement
      checkouts <- Session.statement bucketId getCheckoutEventsForBucket
      return $ DBBucketStats
        { _dbsBucketId = bucketId
        , _dbsBucketType = b ^. bBucketType
        , _dbsSvid = svid
        , _dbsUserCount = users
        , _dbsImpressionCount = impressions
        , _dbsCheckoutEvents = checkouts
        }

validateCampaignSession :: CampaignId -> Session Bool
validateCampaignSession cmpId = do
  mExpId <- Session.statement cmpId getExperimentIdByCampaignId
  return $ case mExpId of
    Just _ -> True
    Nothing -> False
