{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Database.Sessions where

import Control.Monad (forM, forM_)
import Data.Aeson (Value)
import Data.Map.Strict as M
import Data.Text (Text)
import Hasql.Session (Session)
import qualified Hasql.Session as Session
import SweetSpot.Data.Common
import SweetSpot.Database.Statements
import SweetSpot.Data.Domain
import SweetSpot.Data.Api
import Control.Lens ((^.))

-- | ---------------------------------------------------------------------------
-- | Injectable sessions
-- | ---------------------------------------------------------------------------
getUserBucketSession :: UserId -> Session [UserBucket]
getUserBucketSession userId = Session.statement userId userBucketStatement

assignAndGetUserBucketSession :: CampaignId -> Maybe UserId -> BucketType -> Session [UserBucket]
assignAndGetUserBucketSession cmpId mUid bucketType = do
  uid <- case mUid of
    Just id -> pure id
    Nothing -> Session.statement () insertUserStatement
  insertedCmpId <- Session.statement (uid, cmpId) assignUserToCampaignStatement
  bs <- Session.statement (insertedCmpId, bucketType) bucketByTypePerExpInCampaignStatement
  forM_
    bs
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
  forM experiments addBuckets
  where
    addBuckets experiment = do
      let experimentId = experiment ^. eExpId
      bs <- Session.statement experimentId getBucketsForExperimentStatement
      return
        ExperimentBuckets
          { _ebExpId = experimentId
          , _ebBuckets = bs
          , _ebProductName = experiment ^. eProductName
          , _ebSku = experiment ^. eSku
          }

createExperimentSession :: (Sku, Svid, Svid, Price, Price, CampaignId, Text) -> Session ()
createExperimentSession (sku, controlSvid, testSvid, controlPrice, testPrice, cmp, name) = do
  expId <- Session.statement (sku, name) insertExperimentStatement

  Session.statement (cmp, expId) insertCampaignExperimentStatement

  controlBucketId <-
    Session.statement (Control, controlSvid, controlSvid, controlPrice) insertBucketStatement

  testBucketId <-
    Session.statement (Test, controlSvid, testSvid, testPrice) insertBucketStatement

  Session.statement (expId, controlBucketId) insertExperimentBucketStatement
  Session.statement (expId, testBucketId) insertExperimentBucketStatement


getBucketStats :: Bucket -> Session DBBucketStats
getBucketStats b = do
  let bucketId = b ^. bBucketId
  users <- Session.statement bucketId getBucketUserCountStatement
  impressions <- Session.statement bucketId getBucketImpressionCountStatement
  checkouts <- Session.statement bucketId getCheckoutEventsForBucket
  return DBBucketStats
    { _dbsBucketId = bucketId
    , _dbsBucketType = b ^. bBucketType
    , _dbsOriginalSvid = b ^. bOriginalSvid
    , _dbsTestSvid = b ^. bTestSvid
    , _dbsUserCount = users
    , _dbsImpressionCount = impressions
    , _dbsCheckoutEvents = checkouts
    , _dbsPrice = b ^. bPrice
    }

getExperimentStats :: Experiment -> Session DBExperimentStats
getExperimentStats exp = do
  bs <- Session.statement (exp ^. eExpId) getBucketsForExperimentStatement
  bStats <- mapM getBucketStats bs
  return DBExperimentStats
    { _desExpId = exp ^. eExpId
    , _desProductName = exp ^. eProductName
    , _desBuckets = bStats
    }

getCampaignStatsSession :: CampaignId -> Session DBCampaignStats
getCampaignStatsSession cmpId = do
  cmp <- Session.statement cmpId getCampaignStatement
  exps <- Session.statement cmpId getCampaignExperimentsStatement
  expStats <- mapM getExperimentStats exps
  -- lineItemsControl <- Session.statement (cmpId, Control) getLineItemsPerUserForCampaign
  -- lineItemsTest <- Session.statement (cmpId, Test) getLineItemsPerUserForCampaign
  return DBCampaignStats
    { _dcsCampaignId = cmpId
    , _dcsCampaignName = cmp ^. cCampaignName
    , _dcsMinProfitIncrease = cmp ^. cMinProfitIncrease
    , _dcsStartDate = cmp ^. cStartDate
    , _dcsEndDate = cmp ^. cEndDate
    , _dcsExperiments = expStats
    }
  where
    groupByUser :: [(Int, [LineItem])] -> [LineItem]
    groupByUser = mconcat . fmap snd . M.toList . M.fromListWith (<>)
