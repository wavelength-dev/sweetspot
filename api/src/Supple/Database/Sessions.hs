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

-- | ---------------------------------------------------------------------------
-- | Injectable sessions
-- | ---------------------------------------------------------------------------
getUserBucketSession :: UserId -> Session [UserBucket]
getUserBucketSession userId = Session.statement userId userBucketStatement

assignAndGetUserBucketSession :: CampaignId -> Session [UserBucket]
assignAndGetUserBucketSession cmpId = do
  uid <- Session.statement () insertUserStatement
  insertedCmpId <- Session.statement (uid, cmpId) assignUserToCampaignStatement
  randBs <- Session.statement insertedCmpId randomBucketPerExpInCampaignStatement
  forM_
    randBs
    (\(_, bucketId) ->
       Session.statement (uid, bucketId) assignUserToBucketStatement)
  Session.statement uid userBucketStatement


validateCampaignSession :: CampaignId -> Session Bool
validateCampaignSession cmpId = do
  mExpId <- Session.statement cmpId getActiveCampaignById
  return $ case mExpId of
    Just _ -> True
    Nothing -> False

insertEventSession :: (EventType, Value) -> Session ()
insertEventSession input = Session.statement input insertEventStatement

-- | ---------------------------------------------------------------------------
-- | Dashboard sessions
-- | ---------------------------------------------------------------------------
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
            , _ebProductName = experiment ^. eProductName
            , _ebSku = experiment ^. eSku
            }

createExperimentSession :: (Sku, Svid, Svid, Price, CampaignId, Text) -> Session ()
createExperimentSession (sku, orig_svid, test_svid, price, cmp, name) = do
  expId <- Session.statement (sku, name) insertExperimentStatement
  -- TODO: create both test and control here
  bucketId <-
    Session.statement (Test, orig_svid, test_svid, sku, price) insertBucketStatement
  Session.statement (expId, bucketId) insertExperimentBucketStatement

getBucketStats :: Bucket -> Session DBBucketStats
getBucketStats b = do
  let
    bucketId = b ^. bBucketId
  users <- Session.statement bucketId getBucketUserCountStatement
  impressions <- Session.statement bucketId getBucketImpressionCountStatement
  checkouts <- Session.statement bucketId getCheckoutEventsForBucket
  return $ DBBucketStats
    { _dbsBucketId = bucketId
    , _dbsBucketType = b ^. bBucketType
    , _dbsOriginalSvid = b ^. bOriginalSvid
    , _dbsTestSvid = b ^. bTestSvid
    , _dbsUserCount = users
    , _dbsImpressionCount = impressions
    , _dbsCheckoutEvents = checkouts
    }

getExperimentStats :: Experiment -> Session DBExperimentStats
getExperimentStats exp = do
  bs <- Session.statement (exp ^. eExpId) getBucketsForExperimentStatement
  bStats <- mapM getBucketStats bs
  return $ DBExperimentStats
    { _desExpId = exp ^. eExpId
    , _desProductName = exp ^. eProductName
    , _desBuckets = bStats
    }

getCampaignStatsSession :: CampaignId -> Session DBCampaignStats
getCampaignStatsSession cmpId = do
  cmp <- Session.statement cmpId getCampaignStatement
  exps <- Session.statement cmpId getCampaignExperimentsStatement
  expStats <- mapM getExperimentStats exps
  return $ DBCampaignStats
    { _dcsCampaignId = cmpId
    , _dcsCampaignName = cmp ^. cCampaignName
    , _dcsMinProfitIncrease = cmp ^. cMinProfitIncrease
    , _dcsStartDate = cmp ^. cStartDate
    , _dcsEndDate = cmp ^. cEndDate
    , _dcsExperiments = expStats
    }
