{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module SweetSpot.Database.Queries.Injectable
        ( InjectableDB(..)
        )
where

import           Control.Lens            hiding ( (<.)
                                                , (>.)
                                                )


import qualified Data.List                     as L
import           Data.Maybe                     ( Maybe(..)
                                                , fromJust
                                                )

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
                                               as BeamExt
import           Database.Beam.Postgres
import           System.Random                  ( randomRIO )

import           SweetSpot.AppM                 ( AppM(..) )
import           SweetSpot.Database.Schema
                                         hiding ( UserId )
import           SweetSpot.Database.Queries.Util
                                                ( withConn )
import           SweetSpot.Data.Api
import           SweetSpot.Data.Common



class Monad m => InjectableDB m where
  getNewCampaignTestMaps :: CampaignId -> Maybe UserId -> m [TestMap]
  getUserTestMaps :: UserId -> m [TestMap]
  validateCampaign :: CampaignId -> m Bool
  insertCheckoutEvent :: ApiCheckoutEvent -> m ()

instance InjectableDB AppM where
        getNewCampaignTestMaps cmpId mUid = do
                randTreatment <- liftIO $ randomRIO (0 :: Int, 1 :: Int)
                withConn $ \conn -> do
                        uid <- case mUid of
                                Just id -> pure id
                                Nothing -> insertUser conn
                        assignUserToCampaign conn (uid, cmpId, randTreatment)
                        getUserTestMaps' conn uid

        getUserTestMaps uid = withConn $ \conn -> getUserTestMaps' conn uid

        validateCampaign cmpId = withConn $ \conn -> do
                res <-
                        runBeamPostgres conn
                        $ runSelectReturningList
                        $ select
                        $ do
                                  cmps <- all_ (db ^. campaigns)
                                  guard_ (_cmpId cmps ==. val_ cmpId)
                                  guard_ (isCampaignActive cmps)

                                  pure cmps

                return $ not (null res)

        insertCheckoutEvent apiEvent = withConn $ \conn -> do
                [dbEvent] <-
                        runBeamPostgres conn
                        $ BeamExt.runInsertReturningList
                        $ insert (db ^. checkoutEvents)
                        $ insertExpressions
                                  [ CheckoutEvent
                                            { _cevId      = eventId_
                                            , _cevCreated = now_
                                            , _cevCmpId   = val_
                                                            $  CampaignKey
                                                            $  apiEvent
                                                            ^. aceCampaignId
                                            , _cevOrderId = val_
                                                            $  apiEvent
                                                            ^. aceOrderId
                                            , _cevShopId  = val_
                                                            $  ShopKey
                                                            $  apiEvent
                                                            ^. aceShopId
                                            , _cevUserId  = val_
                                                            $  UserKey
                                                            $  apiEvent
                                                            ^. aceUserId
                                            }
                                  ]

                runBeamPostgres conn
                        $ runInsert
                        $ insert (db ^. checkoutItems)
                        $ insertExpressions
                        $ L.map
                                  (\li -> CheckoutItem
                                          { _ciId              = pgGenUUID_
                                          , _ciCheckoutEventId =
                                                  val_
                                                  $  CheckoutEventKey
                                                  $  dbEvent
                                                  ^. cevId
                                          , _ciQuantity        = val_
                                                                 $  li
                                                                 ^. liQuantity
                                          , _ciSvid = val_ $ li ^. liVariantId
                                          }
                                  )
                                  (apiEvent ^. aceItems)

insertUser :: Connection -> IO UserId
insertUser conn = do
        [user] <-
                runBeamPostgres conn
                $ BeamExt.runInsertReturningList
                $ insert (db ^. users)
                $ insertExpressions [User userId_ now_]
        return $ user ^. usrId

assignUserToCampaign :: Connection -> (UserId, CampaignId, Int) -> IO CampaignId
assignUserToCampaign conn (usrId, campaignId, treatment) = do
        [cmpUsr] <-
                runBeamPostgres conn
                $ BeamExt.runInsertReturningList
                $ insert (db ^. userExperiments)
                $ insertValues
                          [ UserExperiment (UserKey usrId)
                                           (CampaignKey campaignId)
                                           treatment
                          ]
        return $ cmpUsr ^. ueCmpId

getUserTestMaps' :: Connection -> UserId -> IO [TestMap]
getUserTestMaps' conn uid = do
        mUserTreatments <-
                runBeamPostgres conn $ runSelectReturningList $ select $ do
                        cmps    <- all_ (db ^. campaigns)
                        usrExps <- all_ (db ^. userExperiments)

                        guard_ ((usrExps ^. ueUserId) ==. val_ uid)
                        guard_ (isCampaignActive cmps)
                        guard_ (_ueCmpId usrExps `references_` cmps)

                        pure usrExps

        case mUserTreatments of
                [usrTreatment] -> do
                        variants <-
                                runBeamPostgres conn
                                $ runSelectReturningList
                                $ select
                                $ do

                                          usrs    <- all_ (db ^. users)
                                          usrExps <- all_
                                                  (db ^. userExperiments)
                                          treats <- all_ (db ^. treatments)
                                          prodVs <- all_ (db ^. productVariants)
                                          cmps   <- all_ (db ^. campaigns)

                                          guard_
                                                  (_ueUserId usrExps
                                                  `references_` usrs
                                                  )
                                          guard_
                                                  (             _ueCmpId usrExps
                                                  `references_` cmps
                                                  )

                                          guard_
                                                  (             _trCmpId treats
                                                  `references_` cmps
                                                  )
                                          guard_
                                                  (_trProductVariantId treats
                                                  `references_` prodVs
                                                  )

                                          guard_ (usrs ^. usrId ==. val_ uid)
                                          guard_ (isCampaignActive cmps)

                                          pure (treats, prodVs)

                        return
                                $ let
                                          treatment =
                                                  usrTreatment ^. ueTreatment
                                          isTreatmentVariant =
                                                  (== treatment)
                                                          . (^. trTreatment)
                                                          . fst
                                          treatmentVariants = filter
                                                  isTreatmentVariant
                                                  variants
                                          nonTreatmentVariants = filter
                                                  (not . isTreatmentVariant)
                                                  variants
                                          toTestMap (_, v) = TestMap
                                                  { userId    = uid
                                                  , targetId  = v ^. pvVariantId
                                                  , sku       = v ^. pvSku
                                                  , swapPrice = v ^. pvPrice
                                                  , swapId    =
                                                          (^. pvVariantId)
                                                          . snd
                                                          . fromJust
                                                          $ L.find
                                                                    ((== (v
                                                                         ^. pvSku
                                                                         )
                                                                     )
                                                                    . (^. pvSku
                                                                      )
                                                                    . snd
                                                                    )
                                                                    treatmentVariants
                                                  }
                                  in
                                          L.map toTestMap nonTreatmentVariants
                _ -> return []


isCampaignActive cmp = maybe_ false_ (<. now_) (cmp ^. cmpStart)
        &&. maybe_ false_ (>. now_) (cmp ^. cmpEnd)
        where false_ = val_ False
