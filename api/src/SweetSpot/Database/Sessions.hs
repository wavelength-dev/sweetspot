{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Database.Sessions where

import Control.Monad (forM)
import Data.Map.Strict as M
import Hasql.Session (Session)
import qualified Hasql.Session as Session
import SweetSpot.Data.Common
import SweetSpot.Database.Statements
import SweetSpot.Data.Domain
import SweetSpot.Data.Api
import Control.Lens ((^.))

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
