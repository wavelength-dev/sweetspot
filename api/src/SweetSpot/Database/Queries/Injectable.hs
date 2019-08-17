{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SweetSpot.Database.Queries.Injectable where

import           Control.Lens            hiding ( (<.)
                                                , (>.)
                                                )
import           Database.Beam
import           Database.Beam.Backend.SQL.Types
                                                ( SqlSerial(..) )
import           Database.Beam.Postgres
import           Data.Scientific                ( fromFloatDigits )

import           SweetSpot.Database.Schema
                                         hiding ( UserId )
import           SweetSpot.Data.Api             ( UserBucket(..) )
import           SweetSpot.Data.Common

-- addUsers :: Connection -> IO ()
-- addUsers conn =
--         runBeamPostgresDebug putStrLn conn
--                 $ runInsert
--                 $ insert (_users db)
--                 $ insertExpressions [User default_, User default_]

-- getAllBuckets :: Connection -> IO [Bucket]
-- getAllBuckets conn =
--         runBeamPostgresDebug putStrLn conn $ runSelectReturningList $ select
--                 allBuckets
--         where allBuckets = all_ (_buckets db)


-- getAllUsers :: Connection -> IO [User]
-- getAllUsers conn =
--         runBeamPostgresDebug putStrLn conn $ runSelectReturningList $ select
--                 allUsers
--         where allUsers = all_ (_users db)

-- filter_ (\c -> _cmpId c ==. val_ cmpId)


getUserBuckets :: Connection -> UserId -> IO [UserBucket]
getUserBuckets conn uid@(UserId id) = do
        tuples <-
                runBeamPostgresDebug putStrLn conn
                $ runSelectReturningList
                $ select
                $ do

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

                          guard_ (_usrId usrs ==. val_ (SqlSerial id))
                          guard_ (cmps ^. cmpStartDate <. now_)
                          guard_ (cmps ^. cmpEndDate >. now_)

                          pure (exps, bkts)

        return $ fmap
                (\(exp, bkt) -> UserBucket
                        { _ubUserId       = uid
                        , _ubSku          = Sku $ exp ^. expSku
                        , _ubOriginalSvid = Svid $ bkt ^. bktControlSvid
                        , _ubTestSvid     = Svid $ bkt ^. bktTestSvid
                        , _ubPrice = Price $  bkt ^. bktPrice
                        , _ubExpId        = ExpId $ exp ^. expId & unSerial
                        , _ubBucketId     = BucketId $ bkt ^. bktId & unSerial
                        , _ubBucketType   = bkt ^. bktType & bucketTypeFromText
                        }
                )
                tuples
