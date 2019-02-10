{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Supple.Database.Sessions where

import Control.Monad (forM, forM_)
import Data.Int (Int64)
import Hasql.Session (Session)
import Data.Text (Text)
import qualified Hasql.Session as Session
import Supple.Database.Statements
import Supple.Data.Database
import Supple.Data.Common (EventType, Price)

getUserBucketSession :: Int64 -> Session [UserBucket]
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
        let id = exp_id (exp :: Experiment)
        bs <-
          Session.statement (fromIntegral id) getBucketsForExperimentStatement
        return
          ExperimentBuckets
            { exp_id = id
            , sku = sku (exp :: Experiment)
            , name = name (exp :: Experiment)
            , buckets = bs
            }

insertEventSession :: (EventType, TrackViewJSON) -> Session ()
insertEventSession input = Session.statement input insertEventStatement

createExperimentSession :: (Text, Int64, Price) -> Session ()
createExperimentSession (sku, svid, price) = do
  expId <- Session.statement (sku, "test experiment") insertExperimentStatement
  bucketId <- Session.statement (svid, sku, price) insertBucketStatement
  Session.statement (expId, bucketId) insertExperimentBucketStatement
