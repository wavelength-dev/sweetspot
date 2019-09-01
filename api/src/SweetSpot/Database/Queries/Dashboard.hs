{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Database.Queries.Dashboard
        ( createExperiment
        , getCampaignStats
        , getDashboardExperiments
        )
where

import           Control.Lens
import           Data.Aeson                     ( fromJSON
                                                , Result(..)
                                                )
import           Data.Aeson.Lens
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
                                               as BeamExt
import           Database.Beam.Postgres

import qualified SweetSpot.Data.Api            as Api
import           SweetSpot.Data.Common
import           SweetSpot.Data.Domain   hiding ( Campaign )
import           SweetSpot.Database.Schema


getUserId evs = cast_ ((evs ^. evPayload) ->$ "userId") int

createExperiment
        :: Connection
        -> (Sku, Svid, Svid, Price, Price, CampaignId, Text)
        -> IO ()
createExperiment conn (Sku s, Svid ctrl, Svid test, Price ctrlP, Price testP, CampaignId c, name)
        = do
                [exp] <-
                        runBeamPostgres conn
                        $ BeamExt.runInsertReturningList
                        $ insert (db ^. experiments)
                        $ insertExpressions
                                  [ Experiment { _expId          = default_
                                               , _expSku         = val_ s
                                               , _expProductName = val_ name
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
                                           , _bktType      = val_ "control"
                                           , _bktCtrlSvid  = val_ ctrl
                                           , _bktTestSvid  = val_ ctrl
                                           , _bktCtrlPrice = val_ ctrlP
                                           , _bktPrice     = val_ ctrlP
                                           }
                                  , Bucket { _bktId        = default_
                                           , _bktType      = val_ "test"
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
                                  [ ExperimentBucket { _expForBkt = toExpKey exp
                                                     , _bktForExp = toBktKey cb
                                                     }
                                  , ExperimentBucket { _expForBkt = toExpKey exp
                                                     , _bktForExp = toBktKey tb
                                                     }
                                  ]
    where
        toExpKey exp = exp ^. expId & ExperimentKey
        toBktKey bkt = bkt ^. bktId & BucketKey


getDashboardExperiments :: Connection -> IO [Api.ExperimentBuckets]
getDashboardExperiments conn = do
        exps <- runBeamPostgres conn $ runSelectReturningList $ select $ all_
                (db ^. experiments)
        traverse addBuckets exps
    where
        toApiBucket b = Api.Bucket
                { Api._bBucketId     = b ^. bktId & unSerial & BucketId
                , Api._bBucketType   = b ^. bktType & bucketTypeFromText
                , Api._bOriginalSvid = b ^. bktCtrlSvid & Svid
                , Api._bTestSvid     = b ^. bktTestSvid & Svid
                , Api._bControlPrice = b ^. bktCtrlPrice & Price
                , Api._bPrice        = b ^. bktPrice & Price
                }
        addBuckets exp = do
                let id = exp ^. expId & unSerial & ExpId
                bs <- getBucketsForExperiment conn id
                return Api.ExperimentBuckets
                        { Api._ebExpId       = id
                        , Api._ebBuckets     = toApiBucket <$> bs
                        , Api._ebProductName = exp ^. expProductName
                        , Api._ebSku         = exp ^. expSku & Sku
                        }

-- | ---------------------------------------------------------------------------
-- | Stats
-- | ---------------------------------------------------------------------------
getCampaign :: Connection -> CampaignId -> IO (Maybe Campaign)
getCampaign conn (CampaignId id) =
        runBeamPostgres conn
                $ runSelectReturningOne
                $ select
                $ filter_ (\c -> _cmpId c ==. val_ id)
                $ all_ (_campaigns db)


getCampaignExperiments :: Connection -> CampaignId -> IO [Experiment]
getCampaignExperiments conn (CampaignId cmpId) =
        runBeamPostgres conn $ runSelectReturningList $ select $ do
                exps    <- all_ (db ^. experiments)
                cmpExps <- all_ (db ^. campaignExperiments)

                guard_ (_expForCmp cmpExps `references_` exps)
                guard_ (_cmpForExp cmpExps ==. cmpKey)

                pure exps
        where cmpKey = val_ (CampaignKey cmpId)

getExperimentBuckets :: Connection -> ExpId -> IO [Bucket]
getExperimentBuckets conn (ExpId eid) =
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
getBucketUserCount conn (BucketId id) = do
        mCount <-
                runBeamPostgres conn
                $ runSelectReturningOne
                $ select
                $ aggregate_
                          (countOver_ distinctInGroup_ . (^. bktForUsr))
                          (distinctUsersInBucket bktKey)
        return $ fromMaybe 0 mCount
        where bktKey = val_ $ SqlSerial id


toCheckoutEvent :: (Event, BucketUser) -> CheckoutEvent
toCheckoutEvent (e, bu) = CheckoutEvent
        { _chkId        = EventId $ e ^. evId & unSerial
        , _chkUserId    = uid
        , _chkBucketId  = BucketId $ bu ^. bktForUsr & unSerial
        , _chkOrderId   = oid
        , _chkTimestamp = e ^. evTimestamp
        , _chkLineItems = pl ^?! key "lineItems" & parseLineItems
        }
    where
        (PgJSONB pl) = _evPayload e
        uid          = UserId $ pl ^?! key "userId" . _Integral & fromIntegral
        oid          = OrderId $ pl ^?! key "orderId" . _Integral & fromIntegral

        parseLineItems v = case fromJSON v of
                Success r   -> r
                Error   err -> error err


getCheckoutEventsForBucket :: Connection -> BucketId -> IO [CheckoutEvent]
getCheckoutEventsForBucket conn (BucketId id) = do
        events <- runBeamPostgres conn $ runSelectReturningList $ select $ do
                evs <- all_ (db ^. events)
                bu  <- filter_
                        (\bu -> (bu ^. usrForBkt) ==. getUserId evs)
                        (all_ (db ^. bucketUsers))

                guard_ (evs ^. evType ==. "checkout")
                guard_ (bu ^. bktForUsr ==. bktKey)

                pure (evs, bu)
        return $ fmap toCheckoutEvent events
        where bktKey = val_ $ SqlSerial id

getBucketImpressionCount :: Connection -> Bucket -> IO Int
getBucketImpressionCount conn b = do
        [count] <-
                runBeamPostgres conn
                $ runSelectReturningList
                $ select
                $ aggregate_ (const countAll_)
                $ do
                          evs <- filter_ ((==. "view") . (^. evType))
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
        chkEvs     <- getCheckoutEventsForBucket conn (BucketId bid)
        [usrCount] <-
                runBeamPostgres conn
                $ runSelectReturningList
                $ select
                $ aggregate_ (const countAll_)
                $ pgNubBy_ _usrForBkt
                $ filter_ ((==. val_ (SqlSerial bid)) . (^. bktForUsr))
                          (all_ (db ^. bucketUsers))

        imprCount <- getBucketImpressionCount conn b

        return $ DBBucketStats
                { _dbsBucketId        = BucketId bid
                , _dbsBucketType      = bucketTypeFromText $ b ^. bktType
                , _dbsOriginalSvid    = Svid $ b ^. bktCtrlSvid
                , _dbsTestSvid        = Svid $ b ^. bktTestSvid
                , _dbsUserCount       = usrCount
                , _dbsImpressionCount = imprCount
                , _dbsCheckoutEvents  = chkEvs
                , _dbsPrice           = Price $ b ^. bktPrice
                }

getBucketsForExperiment :: Connection -> ExpId -> IO [Bucket]
getBucketsForExperiment conn (ExpId eid) =
        runBeamPostgres conn $ runSelectReturningList $ select $ do
                ebs <- all_ (db ^. experimentBuckets)
                bs  <- all_ (db ^. buckets)

                guard_ (_bktForExp ebs `references_` bs)
                guard_ ((ebs ^. expForBkt) ==. val_ (SqlSerial eid))

                pure bs

getExperimentStats :: Connection -> Experiment -> IO DBExperimentStats
getExperimentStats conn exp = do
        let     id  = unSerial $ exp ^. expId
                eid = ExpId id
        bs     <- getBucketsForExperiment conn eid
        bStats <- traverse (getBucketStats conn) bs
        return DBExperimentStats { _desExpId       = eid
                                 , _desProductName = exp ^. expProductName
                                 , _desBuckets     = bStats
                                 }

getCampaignStats :: Connection -> CampaignId -> IO DBCampaignStats
getCampaignStats conn cmpId = do
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
