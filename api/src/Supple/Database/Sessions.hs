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
import Supple.Data.Api
import Control.Lens ((^.), (^..), (&))

getUserBucketSession :: UserId -> Session [UserBucket]
getUserBucketSession userId = Session.statement userId userBucketsStatement

assignAndGetUserBucketSession :: Session [UserBucket]
assignAndGetUserBucketSession = do
  uid <- Session.statement () insertUserStatement
  randBs <- Session.statement () randomBucketPerExpStatement
  forM_
    randBs
    (\(_, bucketId) ->
       Session.statement (uid, bucketId) assignUserToBucketStatement)
  Session.statement uid userBucketsStatement

getBucketsSession :: Session [ExperimentBuckets]
getBucketsSession = do
  exps <- Session.statement () getExperimentsStatement
  expBuckets <- forM exps addBuckets
  return expBuckets
  where
    addBuckets =
      \exp -> do
        let id = exp ^. eExpId
        bs <- Session.statement id getBucketsForExperimentStatement
        return
          ExperimentBuckets
            { _ebExpId = id
            , _ebSku = exp ^. eSku
            , _ebName = exp ^. eName
            , _ebCampaignId = exp ^. eCampaignId
            , _ebBuckets = bs
            }

insertEventSession :: (EventType, Value) -> Session ()
insertEventSession input = Session.statement input insertEventStatement

createExperimentSession :: (Sku, Svid, Price, CampaignId, Text) -> Session ()
createExperimentSession (sku, svid, price, cmp, name) = do
  expId <- Session.statement (sku, cmp, name) insertExperimentStatement
  bucketId <- Session.statement (svid, sku, price) insertBucketStatement
  Session.statement (expId, bucketId) insertExperimentBucketStatement

getExperimentStatsSession :: ExpId -> Session ExperimentStats
getExperimentStatsSession expId = do
  exp <- Session.statement expId getExperimentStatement
  bs <- Session.statement expId getBucketsForExperimentStatement
  bstats <- mapM getBucketStats bs
  return $ ExperimentStats
    { _esExpId = exp ^. eExpId
    , _esUserCount = bstats ^.. traverse . bsUserCount & sum
    , _esImpressionCount = bstats ^.. traverse . bsImpressionCount & sum
    , _esBuckets = bstats
    }

  where
    getBucketStats :: Bucket -> Session BucketStats
    getBucketStats b = do
      let id = b ^. bBucketId
      users <- Session.statement id getBucketUserCountStatement
      impressions <- Session.statement id getBucketImpressionCountStatement
      conversions <- Session.statement id getBucketConversionCountStatement
      return $ BucketStats
        { _bsBucketId = id
        , _bsUserCount = users
        , _bsImpressionCount = impressions
        , _bsConversionCount = conversions
        }
