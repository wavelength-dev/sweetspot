{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Database.Queries.Dashboard where

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

import           SweetSpot.Data.Common
import           SweetSpot.Data.Domain   hiding ( Campaign )
import           SweetSpot.Database.Schema

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


distinctUsersInBucket bktKey = pgNubBy_ (^. bktForUsr) $ filter_
        ((==. bktKey) . (^. bktForUsr))
        (all_ (db ^. bucketUsers))

getBucketUserCount :: Connection -> BucketId -> IO Int
getBucketUserCount conn (BucketId id) = do
        mCount <-
                runBeamPostgres conn
                $ runSelectReturningOne
                $ select
                $ aggregate_
                          (\bktUsr -> countOver_ distinctInGroup_
                                                 (bktUsr ^. bktForUsr)
                          )
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
                        (\bu ->
                                (bu ^. usrForBkt)
                                        ==. cast_
                                                    (   (evs ^. evPayload)
                                                    ->$ "userId"
                                                    )
                                                    serial
                        )
                        (all_ (db ^. bucketUsers))

                guard_ (evs ^. evType ==. "checkout")
                guard_ (bu ^. bktForUsr ==. bktKey)

                pure (evs, bu)
        return $ fmap toCheckoutEvent events
        where bktKey = val_ $ SqlSerial id

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
                $ filter_
                          (\bu -> _bktForUsr bu
                                  ==. BucketKey (val_ (SqlSerial bid))
                          )
                          (all_ (db ^. bucketUsers))

        return $ DBBucketStats
                { _dbsBucketId        = BucketId bid
                , _dbsBucketType      = bucketTypeFromText $ b ^. bktType
                , _dbsOriginalSvid    = Svid $ b ^. bktCtrlSvid
                , _dbsTestSvid        = Svid $ b ^. bktTestSvid
                , _dbsUserCount       = usrCount
                , _dbsImpressionCount = 0
                , _dbsCheckoutEvents  = chkEvs
                , _dbsPrice           = Price $ b ^. bktPrice
                }

getBucketsForExperiment :: Connection -> ExpId -> IO [Bucket]
getBucketsForExperiment conn (ExpId eid) =
        runBeamPostgres conn $ runSelectReturningList $ select $ do
                ebs <- all_ (db ^. experimentBuckets)
                bs  <- all_ (db ^. buckets)

                guard_ (_bktForExp ebs `references_` bs)
                guard_ (_expForBkt ebs ==. ExperimentKey (val_ (SqlSerial eid)))

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
