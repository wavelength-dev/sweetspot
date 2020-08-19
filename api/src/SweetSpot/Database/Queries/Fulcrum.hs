module SweetSpot.Database.Queries.Fulcrum
  ( FulcrumDB (..),
    validateDomain,
  )
where

import Data.Aeson (toJSON)
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Maybe
  ( Maybe (..),
    fromJust,
  )
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions as BeamExt
import Database.Beam.Postgres
import RIO
import SweetSpot.AppM (AppM (..))
import SweetSpot.Data.Api hiding (productVariants)
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Util
  ( matchShop,
    selectShopMoneyFormat,
    unsafeFindShopId,
    withConn,
  )
import SweetSpot.Database.Schema hiding (UserId)
import SweetSpot.Shopify.Types hiding (productVariants)
import SweetSpot.Util (formatPrice)
import System.Random (randomRIO)

class Monad m => FulcrumDB m where
  getUserTestMaps :: ShopDomain -> UserId -> m [TestMap]
  validateCampaign :: CampaignId -> m Bool
  validateShopDomain :: ShopDomain -> m (Maybe ShopId)
  insertUserCartToken :: CartTokenReq -> m ()
  validateUserCartToken :: CartToken -> m (Maybe (ShopId, CampaignId, UserId))
  insertOrder :: ShopId -> CampaignId -> UserId -> Order -> m ()
  insertUnaccountedOrder :: ShopDomain -> Order -> m ()

instance FulcrumDB AppM where
  getUserTestMaps domain uid = withConn $ \conn -> do
    maps <- getUserTestMaps' conn domain uid
    if not (null maps)
      then pure maps
      else do
        mCampaignId <- getLatestCampaign conn domain
        maybe
          (pure [])
          (\cid -> getNewCampaignTestMaps conn domain cid uid)
          mCampaignId

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

  insertUserCartToken req = withConn $ \conn -> do
    let userId = req ^. cartTokenReqUser
    userExists <-
      isJust
        <$> ( runBeamPostgres conn
                $ runSelectReturningOne
                $ select
                $ filter_
                  (view usrId >>> (==. val_ userId))
                  (all_ (db ^. users))
            )
    when userExists $ do
      mToken <- runBeamPostgres conn
        $ runSelectReturningOne
        $ select
        $ do
          cartToken <- all_ (db ^. userCartTokens)
          guard_ (cartToken ^. cartTokenId ==. val_ (req ^. cartTokenReqToken))
          pure $ cartToken ^. cartTokenId
      case mToken of
        Just _ -> pure ()
        Nothing ->
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

  insertUnaccountedOrder shopDomain order = withConn $ \conn ->
    runBeamPostgres conn $ do
      shopId <- unsafeFindShopId shopDomain
      runInsert
        $ insert (db ^. unaccountedOrders)
        $ insertExpressions
          [ UnaccountedOrder
              { _unaccountedOrderId = pgGenUUID_,
                _unaccountedOrderShopId = val_ (ShopKey shopId),
                _unaccountedOrderPayload = val_ (PgJSONB (toJSON order))
              }
          ]

insertUser :: Connection -> UserId -> IO UserId
insertUser conn uid = do
  mUserId <-
    runBeamPostgres conn
      $ runSelectReturningOne
      $ select
      $ do
        user <- all_ (db ^. users)
        guard_ (user ^. usrId ==. val_ uid)
        pure $ user ^. usrId
  case mUserId of
    Just userId -> return userId
    Nothing -> do
      [user] <-
        runBeamPostgres conn
          $ BeamExt.runInsertReturningList
          $ insert (db ^. users)
          $ insertExpressions [User (val_ uid) nowUTC_]
      return $ user ^. usrId

getLatestCampaign :: Connection -> ShopDomain -> IO (Maybe CampaignId)
getLatestCampaign conn domain =
  runBeamPostgres conn
    $ runSelectReturningOne
    $ select
    $ do
      shop <-
        all_ (db ^. shops)
          & filter_ ((==. val_ domain) . view shopDomain)
      campaign <-
        all_ (db ^. campaigns)
          & filter_ isCampaignActive
          & orderBy_ (nullsLast_ . desc_ . view cmpStart)
          & limit_ 1
      guard_ (view cmpShopId campaign ==. view shopId shop)
      pure $ campaign ^. cmpId

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

getUserTestMaps' :: Connection -> ShopDomain -> UserId -> IO [TestMap]
getUserTestMaps' conn domain uid = do
  (Just moneyFormat) <-
    runBeamPostgres conn $ runSelectReturningOne $ select $
      selectShopMoneyFormat domain
  userTreatment <-
    fromJust
      <$> ( runBeamPostgres conn
              $ runSelectReturningOne
              $ select
                ( all_ (db ^. userExperiments)
                    & filter_ (\ue -> ue ^. ueUserId ==. val_ uid)
                    & limit_ 1
                    & fmap (view ueTreatment)
                )
          )
  variants <- runBeamPostgres conn
    $ runSelectReturningList
    $ select
    $ do
      experiment <- all_ (db ^. userExperiments)
      treatment <- all_ (db ^. treatments)
      variant <- all_ (db ^. productVariants)
      campaign <- all_ (db ^. campaigns)
      guard_ (experiment ^. ueUserId ==. val_ uid)
      guard_ (experiment ^. ueCmpId ==. campaign ^. cmpId)
      guard_ (isCampaignActive campaign)
      guard_ (treatment ^. trCmpId ==. campaign ^. cmpId)
      guard_ (treatment ^. trProductVariantId ==. variant ^. pvId)
      pure (treatment, variant)
  let ctrlVariants =
        variants
          & L.filter (fst >>> view trTreatment >>> (== 0))
          & L.sortOn (snd >>> view pvSku)
      testVariants =
        variants
          & L.filter (fst >>> view trTreatment >>> (== 1))
          & L.sortOn (snd >>> view pvSku)
  pure $
    L.zipWith
      ( \(_, cV) (_, tV) ->
          let activeVariant =
                if userTreatment == 0
                  then cV
                  else tV
           in TestMap
                { userId = uid,
                  targetId = cV ^. pvVariantId,
                  sku = cV ^. pvSku,
                  swapPrice = formatPrice moneyFormat (activeVariant ^. pvPrice),
                  swapId = activeVariant ^. pvVariantId
                }
      )
      ctrlVariants
      testVariants

isCampaignActive cmp =
  maybe_ false_ (<. nowUTC_) (cmp ^. cmpStart)
    &&. maybe_ true_ (>. nowUTC_) (cmp ^. cmpEnd)
  where
    false_ = val_ False
    true_ = val_ True

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

getNewCampaignTestMaps ::
  Connection ->
  ShopDomain ->
  CampaignId ->
  UserId ->
  IO [TestMap]
getNewCampaignTestMaps conn domain cmpId userId = do
  randTreatment <- randomRIO (0 :: Int, 1 :: Int)
  uid <- insertUser conn userId
  assignUserToCampaign conn (uid, cmpId, randTreatment)
  getUserTestMaps' conn domain uid
