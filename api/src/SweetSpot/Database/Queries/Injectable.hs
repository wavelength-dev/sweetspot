{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module SweetSpot.Database.Queries.Injectable
        -- ( InjectableDB(..)
        -- )
where

-- import           Control.Lens            hiding ( (<.)
--                                                 , (>.)
--                                                 )
-- import           Control.Monad                  ( forM_ )
-- import           Data.Aeson                     ( Value )
-- import           Data.Maybe                     ( Maybe(..) )
-- import           Database.Beam
-- import           Database.Beam.Backend.SQL.Types
--                                                 ( SqlSerial(..) )
-- import           Database.Beam.Backend.SQL.BeamExtensions
--                                                as BeamExt
-- import           Database.Beam.Postgres
-- import           System.Random                  ( randomRIO )

-- import           SweetSpot.AppM                 ( AppM(..) )
-- import           SweetSpot.Database.Schema
--                                          hiding ( UserId )
-- import           SweetSpot.Database.Queries.Util
--                                                 ( withConn )
-- import           SweetSpot.Data.Api             ( UserBucket(..) )
-- import           SweetSpot.Data.Common



-- class Monad m => InjectableDB m where
--   getNewCampaignBuckets :: CampaignId -> Maybe UserId -> m [UserBucket]
--   getUserBuckets :: UserId -> m [UserBucket]
--   validateCampaign :: CampaignId -> m Bool
--   insertEvent :: (EventType, Value) -> m ()

-- instance InjectableDB AppM where
--         getNewCampaignBuckets cmpId mUid = do
--                 randIdx <- liftIO $ randomRIO (0 :: Int, 1 :: Int)
--                 let bucketType = [Control, Test] !! randIdx
--                 uid <- case mUid of
--                         Just id -> pure id
--                         Nothing -> withConn insertUser
--                 cid <- withConn $ assignUserToCampaign (cmpId, uid)
--                 bs  <- withConn $ bucketByTypePerExpInCampaign (cid, bucketType)
--                 forM_ bs (\(_, bid) -> withConn $ assignUserToBucket (uid, bid))
--                 getUserBuckets uid

--         getUserBuckets uid = withConn $ \conn -> do
--                 tuples <-
--                         runBeamPostgres conn
--                         $ runSelectReturningList
--                         $ select
--                         $ do

--                                   usrs    <- all_ (db ^. users)
--                                   bktUsrs <- all_ (db ^. bucketUsers)
--                                   bkts    <- all_ (db ^. buckets)
--                                   expBkts <- all_ (db ^. experimentBuckets)
--                                   exps    <- all_ (db ^. experiments)
--                                   cmpUsrs <- all_ (db ^. campaignUsers)
--                                   cmps    <- all_ (db ^. campaigns)


--                                   guard_ (_usrForBkt bktUsrs `references_` usrs)
--                                   guard_ (_bktForUsr bktUsrs `references_` bkts)

--                                   guard_ (_bktForExp expBkts `references_` bkts)
--                                   guard_ (_expForBkt expBkts `references_` exps)

--                                   guard_ (_usrForCmp cmpUsrs `references_` usrs)
--                                   guard_ (_cmpForUsr cmpUsrs `references_` cmps)

--                                   guard_
--                                           (usrs ^. usrId ==. val_
--                                                   (SqlSerial uid)
--                                           )
--                                   guard_ (cmps ^. cmpStartDate <. now_)
--                                   guard_ (cmps ^. cmpEndDate >. now_)

--                                   pure (exps, bkts)

--                 return $ fmap
--                         (\(exp, bkt) -> UserBucket
--                                 { _ubUserId       = uid
--                                 , _ubSku          = exp ^. expSku
--                                 , _ubOriginalSvid = bkt ^. bktCtrlSvid
--                                 , _ubTestSvid     = bkt ^. bktTestSvid
--                                 , _ubPrice        = bkt ^. bktPrice
--                                 , _ubExpId        = exp ^. expId & unSerial
--                                 , _ubBucketId     = bkt ^. bktId & unSerial
--                                 , _ubBucketType   = bkt ^. bktType
--                                 , _ubControlPrice = bkt ^. bktCtrlPrice
--                                 }
--                         )
--                         tuples


--         validateCampaign cmpId = withConn $ \conn -> do
--                 res <-
--                         runBeamPostgres conn
--                         $ runSelectReturningList
--                         $ select
--                         $ do
--                                   cmps <- all_ (db ^. campaigns)
--                                   guard_ (_cmpId cmps ==. val_ cmpId)
--                                   guard_ (cmps ^. cmpStartDate <. now_)
--                                   guard_ (cmps ^. cmpEndDate >. now_)

--                                   pure cmps

--                 return $ not (null res)

--         insertEvent (eventType, json) = withConn $ \conn ->
--                 runBeamPostgres conn
--                         $ runInsert
--                         $ insert (db ^. events)
--                         $ insertExpressions
--                                   [ Event { _evId        = default_
--                                           , _evType      = val_ eventType
--                                           , _evTimestamp = now_
--                                           , _evPayload   = val_ $ PgJSONB json
--                                           }
--                                   ]

-- insertUser :: Connection -> IO UserId
-- insertUser conn = do
--         [user] <-
--                 runBeamPostgres conn
--                 $ BeamExt.runInsertReturningList
--                 $ insert (db ^. users)
--                 $ insertExpressions [User default_]
--         return $ user ^. usrId & unSerial

-- assignUserToCampaign :: (CampaignId, UserId) -> Connection -> IO CampaignId
-- assignUserToCampaign (cmpId, usrId) conn = do
--         [cmpUsr] <-
--                 runBeamPostgres conn
--                 $ BeamExt.runInsertReturningList
--                 $ insert (db ^. campaignUsers)
--                 $ insertValues
--                           [ CampaignUser (CampaignKey cmpId)
--                                          (usrId & SqlSerial & UserKey)
--                           ]
--         return $ cmpUsr ^. cmpForUsr

-- assignUserToBucket :: (UserId, BucketId) -> Connection -> IO ()
-- assignUserToBucket (usrId', bktId') conn =
--         runBeamPostgres conn
--                 $ runInsert
--                 $ insert (db ^. bucketUsers)
--                 $ insertValues
--                           [ BucketUser (bktId' & SqlSerial & BucketKey)
--                                        (usrId' & SqlSerial & UserKey)
--                           ]


-- bucketByTypePerExpInCampaign
--         :: (CampaignId, BucketType) -> Connection -> IO [(ExpId, BucketId)]
-- bucketByTypePerExpInCampaign (cid, btype) conn = do
--         res <- runBeamPostgres conn $ runSelectReturningList $ select $ do
--                 cmps    <- all_ (db ^. campaigns)
--                 cmpExps <- all_ (db ^. campaignExperiments)
--                 exps    <- all_ (db ^. experiments)
--                 expBkts <- all_ (db ^. experimentBuckets)
--                 bkts    <- all_ (db ^. buckets)

--                 guard_ (_cmpForExp cmpExps `references_` cmps)
--                 guard_ (_expForCmp cmpExps `references_` exps)

--                 guard_ (_expForBkt expBkts `references_` exps)
--                 guard_ (_bktForExp expBkts `references_` bkts)

--                 guard_ (bkts ^. bktType ==. val_ btype)
--                 guard_ (cmps ^. cmpId ==. val_ cid)

--                 pure (exps, bkts)

--         return $ fmap
--                 (\(exp, bkt) ->
--                         (exp ^. expId & unSerial, bkt ^. bktId & unSerial)
--                 )
--                 res
