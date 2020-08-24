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
import SweetSpot.Database.Schema
import SweetSpot.Shopify.Types
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
          guard_ (_campaignId cmps ==. val_ cmpId)
          guard_ (isCampaignActive cmps)
          pure cmps
    return $ not (null res)

  validateShopDomain shopDomain' = withConn $ \conn ->
    runBeamPostgres conn $ runSelectReturningOne $ select $ do
      shop <- matchShop shopDomain'
      pure $ shop ^. shopId

  insertUserCartToken req = withConn $ \conn -> do
    let uid = req ^. cartTokenReqUser
    userExists <-
      isJust
        <$> ( runBeamPostgres conn
                $ runSelectReturningOne
                $ select
                $ filter_
                  (view userId >>> (==. val_ uid))
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

  insertUnaccountedOrder shopDomain' order = withConn $ \conn ->
    runBeamPostgres conn $ do
      shopId' <- unsafeFindShopId shopDomain'
      runInsert
        $ insert (db ^. unaccountedOrders)
        $ insertExpressions
          [ UnaccountedOrder
              { _unaccountedOrderId = pgGenUUID_,
                _unaccountedOrderShopId = val_ (ShopKey shopId'),
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
        guard_ (user ^. userId ==. val_ uid)
        pure $ user ^. userId
  case mUserId of
    Just uid' -> return uid'
    Nothing -> do
      [user] <-
        runBeamPostgres conn
          $ BeamExt.runInsertReturningList
          $ insert (db ^. users)
          $ insertExpressions [User (val_ uid) nowUTC_]
      return $ user ^. userId

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
          & orderBy_ (nullsLast_ . desc_ . view campaignStart)
          & limit_ 1
      guard_ (view campaignShopId campaign ==. view shopId shop)
      pure $ campaign ^. campaignId

assignUserToCampaign :: Connection -> (UserId, CampaignId, Int) -> IO CampaignId
assignUserToCampaign conn (userId', campaignId', treatment) = do
  [campaignUser] <-
    runBeamPostgres conn
      $ BeamExt.runInsertReturningList
      $ insert (db ^. userExperiments)
      $ insertValues
        [ UserExperiment
            (UserKey userId')
            (CampaignKey campaignId')
            treatment
        ]
  return $ campaignUser ^. userExperimentCampaignId

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
                    & filter_ (\ue -> ue ^. userExperimentUserId ==. val_ uid)
                    & limit_ 1
                    & fmap (view userExperimentTreatment)
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
      guard_ (experiment ^. userExperimentUserId ==. val_ uid)
      guard_ (experiment ^. userExperimentCampaignId ==. campaign ^. campaignId)
      guard_ (isCampaignActive campaign)
      guard_ (treatment ^. treatmentCampaignId ==. campaign ^. campaignId)
      guard_ (treatment ^. treatmentProductVariantId ==. variant ^. productVariantId)
      pure (treatment, variant)
  let ctrlVariants =
        variants
          & L.filter (fst >>> view treatmentKey >>> (== 0))
          & L.sortOn (snd >>> view productVariantSku)
      testVariants =
        variants
          & L.filter (fst >>> view treatmentKey >>> (== 1))
          & L.sortOn (snd >>> view productVariantSku)
  pure $
    L.zipWith
      ( \(_, controlVariant) (_, testVariant) ->
          let activeVariant =
                if userTreatment == 0
                  then controlVariant
                  else testVariant
           in TestMap
                { _testMapUserId = uid,
                  _testMapTargetId = controlVariant ^. productVariantVariantId,
                  _testMapSku = controlVariant ^. productVariantSku,
                  _testMapSwapPrice = formatPrice moneyFormat (activeVariant ^. productVariantPrice),
                  _testMapSwapId = activeVariant ^. productVariantVariantId
                }
      )
      ctrlVariants
      testVariants

isCampaignActive cmp =
  maybe_ false_ (<. nowUTC_) (cmp ^. campaignStart)
    &&. maybe_ true_ (>. nowUTC_) (cmp ^. campaignEnd)
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
      campaign <-
        all_ (db ^. campaigns)
          & filter_ isCampaignActive
      shop <- all_ (db ^. shops)
      guard_ (_cartTokenUser userToken `references_` user)
      guard_ (_userExperimentUserId userExperiment `references_` user)
      guard_ (_userExperimentCampaignId userExperiment `references_` campaign)
      guard_ (_campaignShopId campaign `references_` shop)
      guard_ (userToken ^. cartTokenId ==. val_ token)
      pure (shop ^. shopId, campaign ^. campaignId, user ^. userId)

insertLineItem :: Connection -> EventId -> LineItem -> IO ()
insertLineItem conn eid item =
  runBeamPostgres conn
    $ runInsert
    $ insert (db ^. checkoutItems)
    $ insertExpressions
      [ CheckoutItem
          { _checkoutItemId = pgGenUUID_,
            _checkoutItemCheckoutEventId = val_ $ CheckoutEventKey eid,
            _checkoutItemQuantity = val_ $ item ^. lineItemQuantity,
            _checkoutItemSvid = val_ $ item ^. lineItemVariantId
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
            { _checkoutEventId = eventId_,
              _checkoutEventCreated = val_ $ order ^. orderCreatedAt,
              _checkoutEventCampaignId = val_ $ CampaignKey cid,
              _checkoutEventOrderId = val_ $ order ^. orderId,
              _checkoutEventShopId = val_ $ ShopKey sid,
              _checkoutEventUserId = val_ $ UserKey uid
            }
        ]
  traverse_
    (insertLineItem conn (event ^. checkoutEventId))
    (order ^. orderLineItems)

getNewCampaignTestMaps ::
  Connection ->
  ShopDomain ->
  CampaignId ->
  UserId ->
  IO [TestMap]
getNewCampaignTestMaps conn domain cmpId uid = do
  randTreatment <- randomRIO (0 :: Int, 1 :: Int)
  uid' <- insertUser conn uid
  _ <- assignUserToCampaign conn (uid', cmpId, randTreatment)
  getUserTestMaps' conn domain uid
