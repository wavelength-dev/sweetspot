{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module SweetSpot.Database.Queries.Dashboard
        ( DashboardDB(..)
        , InsertExperiment(..)
        )
where

import           Control.Lens
import           Data.Text                      ( Text )
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
                                               as BeamExt
import           Database.Beam.Postgres

import           SweetSpot.AppM                 ( AppM )

import           SweetSpot.Data.Common

import           SweetSpot.Database.Schema
import           SweetSpot.Database.Queries.Util
                                                ( withConn )

data InsertExperiment = InsertExperiment
  { _insertExperimentSku :: !Sku
  , _insertExperimentSvid :: !Svid
  , _insertExperimentProductId :: !Pid
  , _insertExperimentPrice :: !Price
  , _insertExperimentShopDomain :: !ShopDomain
  , _insertExperimentCampaignId :: !CampaignId
  , _insertExperimentProductName :: !Text
  , _insertExperimentTreatment :: !Int
  }

makeLenses ''InsertExperiment

class Monad m => DashboardDB m where
  createExperiment :: InsertExperiment -> m ()
--   getCampaignStats :: CampaignId -> m DBCampaignStats
--   getDashboardExperiments :: m [Api.ExperimentBuckets]


instance DashboardDB AppM where
  createExperiment args = withConn $ \conn -> do
    let
      domain = args ^. insertExperimentShopDomain

    Just shopId <- runBeamPostgres conn
      $ runSelectReturningOne
      $ select $ do
        row <- filter_ ((==. val_ domain) . (^. shopDomain)) (all_ (db ^. shops))
        pure $ row ^. shopId

    [dbVariant] <- runBeamPostgres conn
      $ runInsertReturningList
      $ insert (db ^. productVariants)
      $ insertExpressions
          [ ProductVariant
              { _pvId = productVariant_
              , _pvShopId = val_ $ ShopKey shopId
              , _pvTitle = val_ $ args ^. insertExperimentProductName
              , _pvSku = val_ $ args ^. insertExperimentSku
              , _pvProductId = val_ $ args ^. insertExperimentProductId
              , _pvVariantId = val_ $ args ^. insertExperimentSvid
              , _pvPrice = val_ $ args ^. insertExperimentPrice
              , _pvCurrency = val_ "USD"
              }
          ]

    runBeamPostgres conn
      $ runInsert
      $ insert (db ^. treatments)
      $ insertExpressions
            [ Treatment
                { _trCmpId = val_ $ CampaignKey $ args ^. insertExperimentCampaignId
                , _trTreatment = val_ $ args ^. insertExperimentTreatment
                , _trProductVariantId = val_ $ PVariantKey $ dbVariant ^. pvId
                }
            ]


--         getCampaignStats cmpId = withConn $ \conn -> do
--                 (Just cmp) <- getCampaign conn cmpId
--                 exps       <- getCampaignExperiments conn cmpId
--                 expStats   <- traverse (getExperimentStats conn) exps
--                 return DBCampaignStats
--                         { _dcsCampaignId        = cmpId
--                         , _dcsCampaignName      = cmp ^. cmpName
--                         , _dcsMinProfitIncrease = cmp ^. cmpMinProfitIncrease
--                         , _dcsStartDate         = cmp ^. cmpStartDate
--                         , _dcsEndDate           = cmp ^. cmpEndDate
--                         , _dcsExperiments       = expStats
--                         }


--         getDashboardExperiments = withConn $ \conn -> do
--                 exps <-
--                         runBeamPostgres conn
--                         $ runSelectReturningList
--                         $ select
--                         $ all_ (db ^. experiments)
--                 traverse (addBuckets conn) exps
--             where
--                 toApiBucket b = Api.Bucket
--                         { Api._bBucketId     = b ^. bktId & unSerial
--                         , Api._bBucketType   = b ^. bktType
--                         , Api._bOriginalSvid = b ^. bktCtrlSvid
--                         , Api._bTestSvid     = b ^. bktTestSvid
--                         , Api._bControlPrice = b ^. bktCtrlPrice
--                         , Api._bPrice        = b ^. bktPrice
--                         }
--                 addBuckets conn exp = do
--                         let id = exp ^. expId & unSerial
--                         bs <- getBucketsForExperiment conn id
--                         return Api.ExperimentBuckets
--                                 { Api._ebExpId       = id
--                                 , Api._ebBuckets     = toApiBucket <$> bs
--                                 , Api._ebProductName = exp ^. expProductName
--                                 , Api._ebSku         = exp ^. expSku
--                                 }


-- getUserId evs = cast_ ((evs ^. evPayload) ->>$ "userId") uidType

-- -- | ---------------------------------------------------------------------------
-- -- | Stats
-- -- | ---------------------------------------------------------------------------
-- getCampaign :: Connection -> CampaignId -> IO (Maybe Campaign)
-- getCampaign conn cid =
--         runBeamPostgres conn
--                 $ runSelectReturningOne
--                 $ select
--                 $ filter_ (\c -> _cmpId c ==. val_ cid)
--                 $ all_ (_campaigns db)


-- getCampaignExperiments :: Connection -> CampaignId -> IO [Experiment]
-- getCampaignExperiments conn cmpId =
--         runBeamPostgres conn $ runSelectReturningList $ select $ do
--                 exps    <- all_ (db ^. experiments)
--                 cmpExps <- all_ (db ^. campaignExperiments)

--                 guard_ (_expForCmp cmpExps `references_` exps)
--                 guard_ (_cmpForExp cmpExps ==. cmpKey)

--                 pure exps
--         where cmpKey = val_ (CampaignKey cmpId)

-- getExperimentBuckets :: Connection -> ExpId -> IO [Bucket]
-- getExperimentBuckets conn eid =
--         runBeamPostgres conn $ runSelectReturningList $ select $ do
--                 expBkts <- all_ (db ^. experimentBuckets)
--                 bkts    <- all_ (db ^. buckets)
--                 guard_ (_bktForExp expBkts `references_` bkts)
--                 guard_ (_expForBkt expBkts ==. expKey)
--                 pure bkts
--         where expKey = val_ (ExperimentKey (SqlSerial eid))


-- distinctUsersInBucket bktKey = pgNubBy_ (^. usrForBkt) $ filter_
--         ((==. bktKey) . (^. bktForUsr))
--         (all_ (db ^. bucketUsers))

-- getBucketUserCount :: Connection -> BucketId -> IO Int
-- getBucketUserCount conn bucketId = do
--         mCount <-
--                 runBeamPostgres conn
--                 $ runSelectReturningOne
--                 $ select
--                 $ aggregate_
--                           (countOver_ distinctInGroup_ . (^. bktForUsr))
--                           (distinctUsersInBucket bktKey)
--         return $ fromMaybe 0 mCount
--         where bktKey = val_ $ SqlSerial bucketId


-- toCheckoutEvent :: (Event, BucketUser) -> CheckoutEvent
-- toCheckoutEvent (e, bu) = CheckoutEvent
--         { _chkId        = e ^. evId & unSerial
--         , _chkUserId    = uid
--         , _chkBucketId  = bu ^. bktForUsr & unSerial
--         , _chkOrderId   = oid
--         , _chkTimestamp = e ^. evTimestamp
--         , _chkLineItems = pl ^?! key "lineItems" & parseLineItems
--         }
--     where
--         (PgJSONB pl) = _evPayload e
--         uid          = UserId $ pl ^?! key "userId" . _Integral & show & pack
--         oid          = OrderId $ pl ^?! key "orderId" . _Integral & fromIntegral

--         parseLineItems v = case fromJSON v of
--                 Success r   -> r
--                 Error   err -> error err


-- getCheckoutEventsForBucket :: Connection -> BucketId -> IO [CheckoutEvent]
-- getCheckoutEventsForBucket conn bucketId = do
--         events <- runBeamPostgres conn $ runSelectReturningList $ select $ do
--                 evs <- all_ (db ^. events)
--                 bu  <- filter_
--                         (\bu -> (bu ^. usrForBkt) ==. getUserId evs)
--                         (all_ (db ^. bucketUsers))

--                 guard_ (evs ^. evType ==. val_ Checkout)
--                 guard_ (bu ^. bktForUsr ==. val_ (SqlSerial bucketId))

--                 pure (evs, bu)
--         return $ fmap toCheckoutEvent events

-- getBucketImpressionCount :: Connection -> Bucket -> IO Int
-- getBucketImpressionCount conn b = do
--         [count] <-
--                 runBeamPostgres conn
--                 $ runSelectReturningList
--                 $ select
--                 $ aggregate_ (const countAll_)
--                 $ do
--                           evs <- filter_ ((==. val_ View) . (^. evType))
--                                   $ all_ (db ^. events)

--                           let     uid = getUserId evs
--                                   bid = val_ (b ^. bktId)

--                           bktUs <- distinctUsersInBucket bid

--                           guard_ (uid ==. (bktUs ^. usrForBkt))

--                           pure evs

--         return count


-- getBucketStats :: Connection -> Bucket -> IO DBBucketStats
-- getBucketStats conn b = do
--         let bid = b ^. bktId & unSerial
--         chkEvs     <- getCheckoutEventsForBucket conn bid
--         [usrCount] <-
--                 runBeamPostgres conn
--                 $ runSelectReturningList
--                 $ select
--                 $ aggregate_ (const countAll_)
--                 $ pgNubBy_ _usrForBkt
--                 $ filter_ ((==. val_ (SqlSerial bid)) . (^. bktForUsr))
--                           (all_ (db ^. bucketUsers))

--         imprCount <- getBucketImpressionCount conn b

--         return $ DBBucketStats { _dbsBucketId        = bid
--                                , _dbsBucketType      = b ^. bktType
--                                , _dbsOriginalSvid    = b ^. bktCtrlSvid
--                                , _dbsTestSvid        = b ^. bktTestSvid
--                                , _dbsUserCount       = usrCount
--                                , _dbsImpressionCount = imprCount
--                                , _dbsCheckoutEvents  = chkEvs
--                                , _dbsPrice           = b ^. bktPrice
--                                }

-- getBucketsForExperiment :: Connection -> ExpId -> IO [Bucket]
-- getBucketsForExperiment conn eid =
--         runBeamPostgres conn $ runSelectReturningList $ select $ do
--                 ebs <- all_ (db ^. experimentBuckets)
--                 bs  <- all_ (db ^. buckets)

--                 guard_ (_bktForExp ebs `references_` bs)
--                 guard_ ((ebs ^. expForBkt) ==. val_ (SqlSerial eid))

--                 pure bs

-- getExperimentStats :: Connection -> Experiment -> IO DBExperimentStats
-- getExperimentStats conn exp = do
--         let eid = exp ^. expId & unSerial
--         bs     <- getBucketsForExperiment conn eid
--         bStats <- traverse (getBucketStats conn) bs
--         return DBExperimentStats { _desExpId       = eid
--                                  , _desProductName = exp ^. expProductName
--                                  , _desBuckets     = bStats
--                                  }
