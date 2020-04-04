module SweetSpot.Database.Queries.Fulcrum
  ( FulcrumDB (..),
    validateDomain,
  )
where

import Control.Lens hiding
  ( (<.),
    (>.),
  )
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Maybe
  ( Maybe (..),
    fromJust,
  )
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions as BeamExt
import Database.Beam.Postgres
import SweetSpot.AppM (AppM (..))
import SweetSpot.Data.Api hiding (productVariants)
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Util
  ( matchShop,
    withConn,
  )
import SweetSpot.Database.Schema hiding (UserId)
import SweetSpot.Shopify.Types
import System.Random (randomRIO)

class Monad m => FulcrumDB m where
  getNewCampaignTestMaps :: CampaignId -> Maybe UserId -> m [TestMap]
  getUserTestMaps :: UserId -> m [TestMap]
  validateCampaign :: CampaignId -> m Bool
  validateShopDomain :: ShopDomain -> m (Maybe ShopId)
  insertUserCartToken :: CartTokenReq -> m ()
  validateUserCartToken :: CartToken -> m (Maybe (ShopId, CampaignId, UserId))
  insertOrder :: ShopId -> CampaignId -> UserId -> Order -> m ()

instance FulcrumDB AppM where
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
      shop <- matchShop shopDomain
      pure $ shop ^. shopId

  insertUserCartToken req = withConn $ \conn ->
    runBeamPostgres conn
      $ runInsert
      $ insert (db ^. userCartTokens)
      $ insertExpressions
        [ UserCartToken
            { _cartTokenId = val_ $ req ^. cartTokenReqToken,
              _cartTokenUser = val_ $ UserKey $ req ^. cartTokenReqUser
            }
        ]

  validateUserCartToken = validateUserCartToken'

  insertOrder = insertOrder'

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
        [ UserExperiment
            (UserKey usrId)
            (CampaignKey campaignId)
            treatment
        ]
  return $ cmpUsr ^. ueCmpId

getUserTestMaps' :: Connection -> UserId -> IO [TestMap]
getUserTestMaps' conn uid = do
  mUserTreatments <-
    runBeamPostgres conn $ runSelectReturningList $ select $ do
      cmps <- all_ (db ^. campaigns)
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
            usrs <- all_ (db ^. users)
            usrExps <- all_ (db ^. userExperiments)
            treats <- all_ (db ^. treatments)
            prodVs <- all_ (db ^. productVariants)
            cmps <- all_ (db ^. campaigns)
            guard_ (_ueUserId usrExps `references_` usrs)
            guard_ (_ueCmpId usrExps `references_` cmps)
            guard_ (_trCmpId treats `references_` cmps)
            guard_ (_trProductVariantId treats `references_` prodVs)
            guard_ (usrs ^. usrId ==. val_ uid)
            guard_ (isCampaignActive cmps)
            pure (treats, prodVs)
      return $
        let treatment =
              usrTreatment ^. ueTreatment
            isTreatmentVariant = (== treatment) . (^. trTreatment) . fst
            treatmentVariants = filter isTreatmentVariant variants
            nonTreatmentVariants = filter (not . isTreatmentVariant) variants
            findSwap v = L.find ((== v ^. pvSku) . (^. pvSku) . snd) treatmentVariants
            toTestMap (_, v) =
              TestMap
                { userId = uid,
                  targetId = v ^. pvVariantId,
                  sku = v ^. pvSku,
                  swapPrice = v ^. pvPrice,
                  swapId = (^. pvVariantId) . snd . fromJust $ findSwap v
                }
         in L.map toTestMap nonTreatmentVariants
    _ -> return []

isCampaignActive cmp =
  maybe_ false_ (<. now_) (cmp ^. cmpStart)
    &&. maybe_ false_ (>. now_) (cmp ^. cmpEnd)
  where
    false_ = val_ False

validateDomain :: Connection -> ShopDomain -> IO (Maybe ShopDomain)
validateDomain conn domain =
  runBeamPostgres conn $ runSelectReturningOne $ select $ do
    row <- matchShop domain
    pure $ row ^. shopDomain

validateUserCartToken' :: CartToken -> AppM (Maybe (ShopId, CampaignId, UserId))
validateUserCartToken' token = withConn $ \conn ->
  runBeamPostgres conn
    $ runSelectReturningOne
    $ select
    $ do
      user <- all_ (db ^. users)
      userToken <- all_ (db ^. userCartTokens)
      userExperiment <- all_ (db ^. userExperiments)
      campaign <- all_ (db ^. campaigns)
      shop <- all_ (db ^. shops)
      guard_ (_cartTokenUser userToken `references_` user)
      guard_ (_ueUserId userExperiment `references_` user)
      guard_ (_ueCmpId userExperiment `references_` campaign)
      guard_ (_cmpShopId campaign `references_` shop)
      guard_ (userToken ^. cartTokenId ==. val_ token)
      pure (shop ^. shopId, campaign ^. cmpId, user ^. usrId)

insertLineItem :: Connection -> EventId -> LineItem -> IO ()
insertLineItem conn eid item =
  runBeamPostgres conn
    $ runInsert
    $ insert (db ^. checkoutItems)
    $ insertExpressions
      [ CheckoutItem
          { _ciId = pgGenUUID_,
            _ciCheckoutEventId = val_ $ CheckoutEventKey eid,
            _ciQuantity = val_ $ item ^. lineItemQuantity,
            _ciSvid = val_ $ item ^. lineItemVariantId
          }
      ]

insertOrder' :: ShopId -> CampaignId -> UserId -> Order -> AppM ()
insertOrder' sid cid uid order = withConn $ \conn -> do
  [event] <-
    runBeamPostgres conn
      $ BeamExt.runInsertReturningList
      $ insert (db ^. checkoutEvents)
      $ insertExpressions
        [ CheckoutEvent
            { _cevId = eventId_,
              _cevCreated = val_ $ order ^. orderCreatedAt,
              _cevCmpId = val_ $ CampaignKey cid,
              _cevOrderId = val_ $ order ^. orderId,
              _cevShopId = val_ $ ShopKey sid,
              _cevUserId = val_ $ UserKey uid
            }
        ]
  traverse_
    (insertLineItem conn (event ^. cevId))
    (order ^. orderLineItems)
