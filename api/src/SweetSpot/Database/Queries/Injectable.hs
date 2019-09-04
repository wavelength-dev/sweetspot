{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SweetSpot.Database.Queries.Injectable
        ( getNewCampaignBuckets
        , getUserBuckets
        , validateCampaign
        , insertEvent
        )
where

import           Control.Lens            hiding ( (<.)
                                                , (>.)
                                                )
import           Control.Monad                  ( forM_ )
import           Data.Aeson                     ( Value )
import           Data.Maybe                     ( Maybe(..) )
import           Database.Beam
import           Database.Beam.Backend.SQL.Types
                                                ( SqlSerial(..) )
import           Database.Beam.Backend.SQL.BeamExtensions
                                               as BeamExt
import           Database.Beam.Postgres
import           System.Random                  ( randomRIO )

import           SweetSpot.Database.Schema
                                         hiding ( UserId )
import           SweetSpot.Data.Api             ( UserBucket(..) )
import           SweetSpot.Data.Common


getUserBuckets :: Connection -> UserId -> IO [UserBucket]
getUserBuckets conn uid = do
        tuples <- runBeamPostgres conn $ runSelectReturningList $ select $ do

                usrs    <- all_ (db ^. users)
                bktUsrs <- all_ (db ^. bucketUsers)
                bkts    <- all_ (db ^. buckets)
                expBkts <- all_ (db ^. experimentBuckets)
                exps    <- all_ (db ^. experiments)
                cmpUsrs <- all_ (db ^. campaignUsers)
                cmps    <- all_ (db ^. campaigns)


                guard_ (_usrForBkt bktUsrs `references_` usrs)
                guard_ (_bktForUsr bktUsrs `references_` bkts)

                guard_ (_bktForExp expBkts `references_` bkts)
                guard_ (_expForBkt expBkts `references_` exps)

                guard_ (_usrForCmp cmpUsrs `references_` usrs)
                guard_ (_cmpForUsr cmpUsrs `references_` cmps)

                guard_ (usrs ^. usrId ==. val_ (SqlSerial uid))
                guard_ (cmps ^. cmpStartDate <. now_)
                guard_ (cmps ^. cmpEndDate >. now_)

                pure (exps, bkts)

        return $ fmap
                (\(exp, bkt) -> UserBucket
                        { _ubUserId       = uid
                        , _ubSku          = exp ^. expSku
                        , _ubOriginalSvid = bkt ^. bktCtrlSvid
                        , _ubTestSvid     = bkt ^. bktTestSvid
                        , _ubPrice        = bkt ^. bktPrice
                        , _ubExpId        = ExpId $ exp ^. expId & unSerial
                        , _ubBucketId     = BucketId $ bkt ^. bktId & unSerial
                        , _ubBucketType   = bkt ^. bktType
                        , _ubControlPrice = bkt ^. bktCtrlPrice
                        }
                )
                tuples

insertUser :: Connection -> IO UserId
insertUser conn = do
        [user] <-
                runBeamPostgres conn
                $ BeamExt.runInsertReturningList
                $ insert (db ^. users)
                $ insertExpressions [User default_]
        return $ user ^. usrId & unSerial

assignUserToCampaign :: Connection -> (CampaignId, UserId) -> IO CampaignId
assignUserToCampaign conn (cmpId, usrId) = do
        [cmpUsr] <-
                runBeamPostgres conn
                $ BeamExt.runInsertReturningList
                $ insert (db ^. campaignUsers)
                $ insertValues
                          [ CampaignUser (CampaignKey cmpId)
                                         (usrId & SqlSerial & UserKey)
                          ]
        return $ cmpUsr ^. cmpForUsr

assignUserToBucket :: Connection -> (UserId, BucketId) -> IO ()
assignUserToBucket conn (usrId', BucketId bktId') =
        runBeamPostgres conn
                $ runInsert
                $ insert (db ^. bucketUsers)
                $ insertValues
                          [ BucketUser (bktId' & SqlSerial & BucketKey)
                                       (usrId' & SqlSerial & UserKey)
                          ]


bucketByTypePerExpInCampaign
        :: Connection -> (CampaignId, BucketType) -> IO [(ExpId, BucketId)]
bucketByTypePerExpInCampaign conn (cid, btype) = do
        res <- runBeamPostgres conn $ runSelectReturningList $ select $ do
                cmps    <- all_ (db ^. campaigns)
                cmpExps <- all_ (db ^. campaignExperiments)
                exps    <- all_ (db ^. experiments)
                expBkts <- all_ (db ^. experimentBuckets)
                bkts    <- all_ (db ^. buckets)

                guard_ (_cmpForExp cmpExps `references_` cmps)
                guard_ (_expForCmp cmpExps `references_` exps)

                guard_ (_expForBkt expBkts `references_` exps)
                guard_ (_bktForExp expBkts `references_` bkts)

                guard_ (bkts ^. bktType ==. (val_ btype))
                guard_ ((cmps ^. cmpId) ==. val_ cid)

                pure (exps, bkts)

        return $ fmap
                (\(exp, bkt) ->
                        ( exp ^. expId & unSerial & ExpId
                        , bkt ^. bktId & unSerial & BucketId
                        )
                )
                res

getNewCampaignBuckets
        :: Connection -> CampaignId -> Maybe UserId -> IO [UserBucket]
getNewCampaignBuckets conn cmpId mUid = do
        randIdx <- randomRIO (0 :: Int, 1 :: Int)
        let bucketType = [Control, Test] !! randIdx
        uid <- case mUid of
                Just id -> pure id
                Nothing -> insertUser conn
        cid <- assignUserToCampaign conn (cmpId, uid)
        bs  <- bucketByTypePerExpInCampaign conn (cid, bucketType)
        forM_ bs (\(_, bid) -> assignUserToBucket conn (uid, bid))
        getUserBuckets conn uid

validateCampaign :: Connection -> CampaignId -> IO Bool
validateCampaign conn cmpId = do
        res <- runBeamPostgres conn $ runSelectReturningList $ select $ do
                cmps <- all_ (db ^. campaigns)
                guard_ (_cmpId cmps ==. val_ cmpId)
                guard_ (cmps ^. cmpStartDate <. now_)
                guard_ (cmps ^. cmpEndDate >. now_)

                pure cmps

        return $ not (null res)

insertEvent :: Connection -> (EventType, Value) -> IO ()
insertEvent conn (eventType, json) =
        runBeamPostgres conn
                $ runInsert
                $ insert (db ^. events)
                $ insertExpressions
                          [ Event { _evId        = default_
                                  , _evType = val_ eventType
                                  , _evTimestamp = now_
                                  , _evPayload   = val_ $ PgJSONB json
                                  }
                          ]
