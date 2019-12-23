{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module SweetSpot.Database.Queries.Injectable
        ( InjectableDB(..)
        , validateDomain
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
import           Data.Text                      ( Text )
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
  validateShopDomain :: ShopDomain -> m (Maybe ShopId)
  insertCheckoutEvent :: ShopId -> ApiCheckoutEvent -> m ()
  generateInstallNonce :: ShopDomain -> m Nonce
  getInstallNonce :: ShopDomain -> m (Maybe Nonce)
  deleteInstallNonce :: ShopDomain -> m ()
  createShop :: ShopDomain -> Text -> m ()

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

        validateShopDomain shopDomain = withConn $ \conn ->
                runBeamPostgres conn $ runSelectReturningOne $ select $ do
                        shops <- all_ (db ^. shops)
                        guard_ (_shopDomain shops ==. val_ shopDomain)
                        pure $ shops ^. shopId

        insertCheckoutEvent shopId apiEvent = withConn $ \conn -> do
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
                                            , _cevShopId = val_ $ ShopKey shopId
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

        generateInstallNonce shopDomain = withConn $ \conn -> do
                [row] <-
                        runBeamPostgres conn
                        $ BeamExt.runInsertReturningList
                        $ insert (db ^. installNonces)
                        $ insertExpressions
                                  [ InstallNonce
                                            { _installNonce      = nonce_
                                            , _installShopDomain =
                                                    val_ shopDomain
                                            }
                                  ]
                pure $ row ^. installNonce

        getInstallNonce shopDomain = withConn $ \conn ->
                runBeamPostgres conn $ runSelectReturningOne $ select $ do
                        rows <- filter_
                                ((==. val_ shopDomain) . (^. installShopDomain))
                                (all_ (db ^. installNonces))
                        pure $ rows ^. installNonce

        deleteInstallNonce shopDomain = withConn $ \conn ->
                runBeamPostgres conn $ runDelete $ delete
                        (db ^. installNonces)
                        ((==. val_ shopDomain) . (^. installShopDomain))

        createShop shopDomain token = withConn $ \conn ->
                runBeamPostgres conn
                        $ runInsert
                        $ insert (db ^. shops)
                        $ insertExpressions
                                  [ Shop { _shopId         = shopId_
                                         , _shopCreated    = now_
                                         , _shopDomain     = val_ shopDomain
                                         , _shopOauthToken = val_ token
                                         }
                                  ]

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

                        guard_ (usrExps ^. ueUserId ==. val_ uid)
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

                                          findSwap v = L.find
                                                  ( (== v ^. pvSku)
                                                  . (^. pvSku)
                                                  . snd
                                                  )
                                                  treatmentVariants

                                          toTestMap (_, v) = TestMap
                                                  { userId    = uid
                                                  , targetId  = v ^. pvVariantId
                                                  , sku       = v ^. pvSku
                                                  , swapPrice = v ^. pvPrice
                                                  , swapId    =
                                                          (^. pvVariantId)
                                                          . snd
                                                          . fromJust
                                                          $ findSwap v
                                                  }
                                  in
                                          L.map toTestMap nonTreatmentVariants
                _ -> return []


isCampaignActive cmp = maybe_ false_ (<. now_) (cmp ^. cmpStart)
        &&. maybe_ false_ (>. now_) (cmp ^. cmpEnd)
        where false_ = val_ False


validateDomain :: Connection -> ShopDomain -> IO (Maybe ShopDomain)
validateDomain conn domain =
        runBeamPostgres conn $ runSelectReturningOne $ select $ do
                row <- filter_ ((==. val_ domain) . (^. shopDomain))
                               (all_ (db ^. shops))
                pure $ row ^. shopDomain
