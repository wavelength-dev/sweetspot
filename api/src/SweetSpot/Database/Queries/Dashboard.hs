{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Database.Queries.Dashboard
        ( DashboardDB(..)
        )
where

import           Control.Lens
import           Data.Aeson                     ( fromJSON
                                                , Result(..)
                                                )
import           Data.Aeson.Lens
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
                                               as BeamExt
import           Database.Beam.Postgres

import           SweetSpot.AppM                 ( AppM )
import qualified SweetSpot.Data.Api            as Api
import           SweetSpot.Data.Common
import           SweetSpot.Data.Domain   hiding ( Campaign )
import           SweetSpot.Database.Schema
import           SweetSpot.Database.Queries.Util
                                                ( withConn )

class Monad m => DashboardDB m where
  createExperiment :: (Sku, Svid, Svid, Price, Price, CampaignId, Text) -> m ()
  getCampaignStats :: CampaignId -> m DBCampaignStats
  getDashboardExperiments :: m [Api.ExperimentBuckets]


instance DashboardDB AppM where
        createExperiment (s, ctrl, test, ctrlP, testP, c, name) =
                withConn $ \conn -> do
                        [exp] <-
                                runBeamPostgres conn
                                $ BeamExt.runInsertReturningList
                                $ insert (db ^. experiments)
                                $ insertExpressions
                                          [ Experiment
                                                    { _expId          = default_
                                                    , _expSku         = val_ s
                                                    , _expProductName =
                                                            val_ name
                                                    }
                                          ]

                        runBeamPostgres conn
                                $ runInsert
                                $ insert (db ^. campaignExperiments)
                                $ insertValues
                                          [ CampaignExperiment
                                                    { _cmpForExp = CampaignKey c
                                                    , _expForCmp = toExpKey exp
                                                    }
                                          ]
                        [cb, tb] <-
                                runBeamPostgres conn
                                $ BeamExt.runInsertReturningList
                                $ insert (db ^. buckets)
                                $ insertExpressions
                                          [ Bucket { _bktId        = default_
                                                   , _bktType = val_ Control
                                                   , _bktCtrlSvid  = val_ ctrl
                                                   , _bktTestSvid  = val_ ctrl
                                                   , _bktCtrlPrice = val_ ctrlP
                                                   , _bktPrice     = val_ ctrlP
                                                   }
                                          , Bucket { _bktId        = default_
                                                   , _bktType      = val_ Test
                                                   , _bktCtrlSvid  = val_ ctrl
                                                   , _bktTestSvid  = val_ test
                                                   , _bktCtrlPrice = val_ ctrlP
                                                   , _bktPrice     = val_ testP
                                                   }
                                          ]

                        runBeamPostgres conn
                                $ runInsert
                                $ insert (db ^. experimentBuckets)
                                $ insertValues
                                          [ ExperimentBucket
                                                  { _expForBkt = toExpKey exp
                                                  , _bktForExp = toBktKey cb
                                                  }
                                          , ExperimentBucket
                                                  { _expForBkt = toExpKey exp
                                                  , _bktForExp = toBktKey tb
                                                  }
                                          ]
            where
                toExpKey exp = exp ^. expId & ExperimentKey
                toBktKey bkt = bkt ^. bktId & BucketKey


        getCampaignStats cmpId = withConn $ \conn -> do
                (Just cmp) <- getCampaign conn cmpId
                exps       <- getCampaignExperiments conn cmpId
                expStats   <- traverse (getExperimentStats conn) exps
                return DBCampaignStats
                        { _dcsCampaignId        = cmpId
                        , _dcsCampaignName      = cmp ^. cmpName
                        , _dcsMinProfitIncrease = cmp ^. cmpMinProfitIncrease
                        , _dcsStartDate         = cmp ^. cmpStartDate
                        , _dcsEndDate           = cmp ^. cmpEndDate
                        , _dcsExperiments       = expStats
                        }


        getDashboardExperiments = withConn $ \conn -> do
                exps <-
                        runBeamPostgres conn
                        $ runSelectReturningList
                        $ select
                        $ all_ (db ^. experiments)
                traverse (addBuckets conn) exps
            where
                toApiBucket b = Api.Bucket
                        { Api._bBucketId     = b ^. bktId & unSerial
                        , Api._bBucketType   = b ^. bktType
                        , Api._bOriginalSvid = b ^. bktCtrlSvid
                        , Api._bTestSvid     = b ^. bktTestSvid
                        , Api._bControlPrice = b ^. bktCtrlPrice
                        , Api._bPrice        = b ^. bktPrice
                        }
                addBuckets conn exp = do
                        let id = exp ^. expId & unSerial
                        bs <- getBucketsForExperiment conn id
                        return Api.ExperimentBuckets
                                { Api._ebExpId       = id
                                , Api._ebBuckets     = toApiBucket <$> bs
                                , Api._ebProductName = exp ^. expProductName
                                , Api._ebSku         = exp ^. expSku
                                }


getUserId evs = cast_ ((evs ^. evPayload) ->>$ "userId") uidType

-- | ---------------------------------------------------------------------------
-- | Stats
-- | ---------------------------------------------------------------------------
getCampaign :: Connection -> CampaignId -> IO (Maybe Campaign)
getCampaign conn cid =
        runBeamPostgres conn
                $ runSelectReturningOne
                $ select
                $ filter_ (\c -> _cmpId c ==. val_ cid)
                $ all_ (_campaigns db)


getCampaignExperiments :: Connection -> CampaignId -> IO [Experiment]
getCampaignExperiments conn cmpId =
        runBeamPostgres conn $ runSelectReturningList $ select $ do
                exps    <- all_ (db ^. experiments)
                cmpExps <- all_ (db ^. campaignExperiments)

                guard_ (_expForCmp cmpExps `references_` exps)
                guard_ (_cmpForExp cmpExps ==. cmpKey)

                pure exps
        where cmpKey = val_ (CampaignKey cmpId)

getExperimentBuckets :: Connection -> ExpId -> IO [Bucket]
getExperimentBuckets conn eid =
        runBeamPostgres conn $ runSelectReturningList $ select $ do
                expBkts <- all_ (db ^. experimentBuckets)
                bkts    <- all_ (db ^. buckets)
                guard_ (_bktForExp expBkts `references_` bkts)
                guard_ (_expForBkt expBkts ==. expKey)
                pure bkts
        where expKey = val_ (ExperimentKey (SqlSerial eid))


distinctUsersInBucket bktKey = pgNubBy_ (^. usrForBkt) $ filter_
        ((==. bktKey) . (^. bktForUsr))
        (all_ (db ^. bucketUsers))

getBucketUserCount :: Connection -> BucketId -> IO Int
getBucketUserCount conn bucketId = do
        mCount <-
                runBeamPostgres conn
                $ runSelectReturningOne
                $ select
                $ aggregate_
                          (countOver_ distinctInGroup_ . (^. bktForUsr))
                          (distinctUsersInBucket bktKey)
        return $ fromMaybe 0 mCount
        where bktKey = val_ $ SqlSerial bucketId


toCheckoutEvent :: (Event, BucketUser) -> CheckoutEvent
toCheckoutEvent (e, bu) = CheckoutEvent
        { _chkId        = e ^. evId & unSerial
        , _chkUserId    = uid
        , _chkBucketId  = bu ^. bktForUsr & unSerial
        , _chkOrderId   = oid
        , _chkTimestamp = e ^. evTimestamp
        , _chkLineItems = pl ^?! key "lineItems" & parseLineItems
        }
    where
        (PgJSONB pl) = _evPayload e
        uid          = UserId $ pl ^?! key "userId" . _Integral & show & pack
        oid          = OrderId $ pl ^?! key "orderId" . _Integral & fromIntegral

        parseLineItems v = case fromJSON v of
                Success r   -> r
                Error   err -> error err


getCheckoutEventsForBucket :: Connection -> BucketId -> IO [CheckoutEvent]
getCheckoutEventsForBucket conn bucketId = do
        events <- runBeamPostgres conn $ runSelectReturningList $ select $ do
                evs <- all_ (db ^. events)
                bu  <- filter_
                        (\bu -> (bu ^. usrForBkt) ==. getUserId evs)
                        (all_ (db ^. bucketUsers))

                guard_ (evs ^. evType ==. val_ Checkout)
                guard_ (bu ^. bktForUsr ==. val_ (SqlSerial bucketId))

                pure (evs, bu)
        return $ fmap toCheckoutEvent events

getBucketImpressionCount :: Connection -> Bucket -> IO Int
getBucketImpressionCount conn b = do
        [count] <-
                runBeamPostgres conn
                $ runSelectReturningList
                $ select
                $ aggregate_ (const countAll_)
                $ do
                          evs <- filter_ ((==. val_ View) . (^. evType))
                                  $ all_ (db ^. events)

                          let     uid = getUserId evs
                                  bid = val_ (b ^. bktId)

                          bktUs <- distinctUsersInBucket bid

                          guard_ (uid ==. (bktUs ^. usrForBkt))

                          pure evs

        return count


getBucketStats :: Connection -> Bucket -> IO DBBucketStats
getBucketStats conn b = do
        let bid = b ^. bktId & unSerial
        chkEvs     <- getCheckoutEventsForBucket conn bid
        [usrCount] <-
                runBeamPostgres conn
                $ runSelectReturningList
                $ select
                $ aggregate_ (const countAll_)
                $ pgNubBy_ _usrForBkt
                $ filter_ ((==. val_ (SqlSerial bid)) . (^. bktForUsr))
                          (all_ (db ^. bucketUsers))

        imprCount <- getBucketImpressionCount conn b

        return $ DBBucketStats { _dbsBucketId        = bid
                               , _dbsBucketType      = b ^. bktType
                               , _dbsOriginalSvid    = b ^. bktCtrlSvid
                               , _dbsTestSvid        = b ^. bktTestSvid
                               , _dbsUserCount       = usrCount
                               , _dbsImpressionCount = imprCount
                               , _dbsCheckoutEvents  = chkEvs
                               , _dbsPrice           = b ^. bktPrice
                               }

getBucketsForExperiment :: Connection -> ExpId -> IO [Bucket]
getBucketsForExperiment conn eid =
        runBeamPostgres conn $ runSelectReturningList $ select $ do
                ebs <- all_ (db ^. experimentBuckets)
                bs  <- all_ (db ^. buckets)

                guard_ (_bktForExp ebs `references_` bs)
                guard_ ((ebs ^. expForBkt) ==. val_ (SqlSerial eid))

                pure bs

getExperimentStats :: Connection -> Experiment -> IO DBExperimentStats
getExperimentStats conn exp = do
        let eid = exp ^. expId & unSerial
        bs     <- getBucketsForExperiment conn eid
        bStats <- traverse (getBucketStats conn) bs
        return DBExperimentStats { _desExpId       = eid
                                 , _desProductName = exp ^. expProductName
                                 , _desBuckets     = bStats
                                 }
