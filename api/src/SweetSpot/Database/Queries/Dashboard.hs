{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module SweetSpot.Database.Queries.Dashboard
  ( DashboardDB (..),
    InsertExperiment (..),
    validateSessionId',
  )
where

import Control.Lens
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Scientific (fromFloatDigits)
import qualified Data.Time.Clock as Clock
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions as BeamExt
import Database.Beam.Postgres
import qualified Database.Beam.Postgres.Full as PG
import RIO hiding (Vector, (^.), view)
import qualified RIO.List as L
import RIO.Partial (fromJust)
import qualified RIO.Vector as V
import Statistics.Sample (mean)
import SweetSpot.AppM (AppM)
import SweetSpot.Data.Api hiding (productVariants)
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Util
  ( matchShop,
    selectShopMoneyFormat,
    unsafeFindShopId,
    withConn,
  )
import SweetSpot.Database.Schema
import SweetSpot.Inference (InfParams (..), runInference)
import SweetSpot.Util
  ( factorToPercentage,
    formatPrice,
    nanToNothing,
    nanToZero,
  )

data InsertExperiment
  = InsertExperiment
      { _insertExperimentSku :: !Sku,
        _insertExperimentSvid :: !Svid,
        _insertExperimentProductId :: !Pid,
        _insertExperimentPrice :: !Price,
        _insertExperimentShopDomain :: !ShopDomain,
        _insertExperimentCampaignId :: !CampaignId,
        _insertExperimentProductName :: !Text,
        _insertExperimentTreatment :: !Int
      }

makeLenses ''InsertExperiment

class Monad m => DashboardDB m where
  createCampaign :: ShopDomain -> CreateCampaign -> m CampaignId
  createExperiment :: InsertExperiment -> m ()
  getCampaigns :: ShopDomain -> m [UICampaign]
  createSession :: ShopDomain -> SessionId -> m ()
  validateSessionId :: SessionId -> m (Maybe ShopDomain)
  unsafeGetShopMoneyFormat :: ShopDomain -> m MoneyFormat
  campaignBelongsToShop :: SessionId -> CampaignId -> m Bool
  stopCampaign :: CampaignId -> m ()
  getTestVariantIds :: CampaignId -> m [Pid]
  clearCampaignCache :: CampaignId -> m ()

instance DashboardDB AppM where
  createCampaign domain cc = withConn $ \conn -> do
    [newCmp] <-
      runBeamPostgres conn $ do
        shopId <- unsafeFindShopId domain
        BeamExt.runInsertReturningList
          $ insert (db ^. campaigns)
          $ insertExpressions
            [ Campaign
                { _campaignId = campaignId_,
                  _campaignShopId = val_ $ ShopKey shopId,
                  _campaignName = val_ $ cc ^. createCampaignName,
                  _campaignStart = just_ nowUTC_,
                  _campaignEnd = val_ $ cc ^. createCampaignEnd
                }
            ]
    pure $ newCmp ^. campaignId

  createExperiment args = withConn $ \conn -> do
    let domain = args ^. insertExperimentShopDomain
    runBeamPostgres conn $ do
      shopId <- unsafeFindShopId domain
      [dbVariant] <-
        runInsertReturningList
          $ insert (db ^. productVariants)
          $ insertExpressions
            [ ProductVariant
                { _productVariantId = productVariant_,
                  _productVariantShopId = val_ $ ShopKey shopId,
                  _productVariantTitle = val_ $ args ^. insertExperimentProductName,
                  _productVariantSku = val_ $ args ^. insertExperimentSku,
                  _productVariantProductId = val_ $ args ^. insertExperimentProductId,
                  _productVariantVariantId = val_ $ args ^. insertExperimentSvid,
                  _productVariantPrice = val_ $ args ^. insertExperimentPrice,
                  _productVariantCurrency = val_ "USD"
                }
            ]
      runInsert
        $ insert (db ^. treatments)
        $ insertExpressions
          [ Treatment
              { _treatmentCampaignId = val_ $ CampaignKey $ args ^. insertExperimentCampaignId,
                _treatmentKey = val_ $ args ^. insertExperimentTreatment,
                _treatmentProductVariantId =
                  val_
                    $ PVariantKey
                    $ dbVariant ^. productVariantId
              }
          ]

  getCampaigns domain = withConn $ \conn -> do
    cmps <-
      runBeamPostgres conn
        $ runSelectReturningList
        $ select
        $ selectShopCampaigns domain
    traverse (readCacheOrEnhance conn domain) cmps

  clearCampaignCache cmpId = withConn $ \conn ->
    runBeamPostgres conn
      $ runDelete
      $ delete (db ^. statsCaches) (\c -> c ^. statsCacheCampaignId ==. val_ cmpId)

  createSession shopDomain' sessionId' = withConn $ \conn -> do
    mShopDomain <- validateSessionId' conn sessionId'
    case mShopDomain of
      Nothing -> do
        shopId <- runBeamPostgres conn
          $ runSelectReturningOne
          $ select
          $ do
            shop <- filter_ ((==. val_ shopDomain') . (^. shopDomain)) (all_ (db ^. shops))
            pure $ shop ^. shopId
        case shopId of
          Just shopId' ->
            runBeamPostgres conn
              $ runInsert
              $ insert (db ^. sessions)
              $ insertExpressions
                [ Session
                    { _sessionId = val_ sessionId',
                      _sessionShopId = val_ $ ShopKey shopId'
                    }
                ]
          -- I don't see how this could ever happen
          Nothing -> error "Tried to create session for non-existent shop"
      Just _ -> return ()

  validateSessionId sessionId' = withConn $ \conn ->
    validateSessionId' conn sessionId'

  unsafeGetShopMoneyFormat domain = withConn $ \conn ->
    -- If you call an dashboard endpoint then you have a session,
    -- which means you have an install which means you'll have a money format
    fromJust
      <$> ( runBeamPostgres conn
              $ runSelectReturningOne
              $ select
              $ selectShopMoneyFormat domain
          )

  campaignBelongsToShop sessionId' cmpId' = withConn $ \conn ->
    isJust
      <$> ( runBeamPostgres conn
              $ runSelectReturningOne
              $ select
              $ do
                session <- all_ (db ^. sessions)
                shop <- all_ (db ^. shops)
                campaign <- all_ (db ^. campaigns)
                guard_ (session ^. sessionId ==. val_ sessionId')
                guard_ (campaign ^. campaignId ==. val_ cmpId')
                guard_ (_sessionShopId session `references_` shop)
                guard_ (_campaignShopId campaign `references_` shop)
                pure $ campaign ^. campaignId
          )

  stopCampaign cmpId' = withConn $ \conn ->
    runBeamPostgres conn
      $ runUpdate
      $ update
        (db ^. campaigns)
        (\cmp -> cmp ^. campaignEnd <-. just_ nowUTC_)
        (\cmp -> cmp ^. campaignId ==. val_ cmpId')

  getTestVariantIds cmpId' = withConn $ \conn ->
    runBeamPostgres conn
      $ runSelectReturningList
      $ select
      $ view productVariantProductId <$> selectUITreatmentVariants cmpId' 1

readCacheOrEnhance :: Connection -> ShopDomain -> Campaign -> IO UICampaign
readCacheOrEnhance conn domain campaign = do
  mCache <- readStatsCache conn (campaign ^. campaignId)
  case mCache of
    Just cached -> pure cached
    Nothing -> do
      uiCampaign <- enhanceCampaign conn domain campaign
      upsertStatsCache conn uiCampaign
      pure uiCampaign

enhanceCampaign :: Connection -> ShopDomain -> Campaign -> IO UICampaign
enhanceCampaign conn domain cmp = do
  let cmpId' = cmp ^. campaignId
  (Just moneyFormat) <-
    runBeamPostgres conn $ runSelectReturningOne $ select $ selectShopMoneyFormat domain
  [(ctrlRevs, ctrlNils, testRevs, testNils)] <-
    runBeamPostgres conn
      $ runSelectReturningList
      $ select
      $ selectCampaignInfParams cmpId'
  let ctrlConvLen = V.length ctrlRevs
      ctrlCR = fromIntegral ctrlConvLen / fromIntegral (ctrlConvLen + ctrlNils)
      ctrlAOV = mean ctrlRevs
      testConvLen = V.length testRevs
      testCR = fromIntegral testConvLen / fromIntegral (testConvLen + testNils)
      testAOV = mean testRevs
      toUITreatmentVariant v =
        UITreatmentVariant
          { _uiTreatmentVariantTitle = v ^. productVariantTitle,
            _uiTreatmentSku = v ^. productVariantSku,
            _uiTreatmentVariantPrice = formatPrice moneyFormat $ v ^. productVariantPrice
          }
  infResult <-
    runInference
      (InfParams (V.convert ctrlRevs) ctrlNils)
      (InfParams (V.convert testRevs) testNils)
  ctrlTreatmentVariants <-
    runBeamPostgres conn
      $ runSelectReturningList
      $ select
      $ selectUITreatmentVariants cmpId' 0
  testTreatmentVariants <-
    runBeamPostgres conn
      $ runSelectReturningList
      $ select
      $ selectUITreatmentVariants cmpId' 1
  updatedAt <- Clock.getCurrentTime
  return
    UICampaign
      { _uiCampaignId = cmp ^. campaignId,
        _uiCampaignName = cmp ^. campaignName,
        _uiCampaignStart = cmp ^. campaignStart,
        _uiCampaignEnd = cmp ^. campaignEnd,
        _uiCampaignLift = infResult,
        _uiCampaignAOVChange = nanToNothing $ factorToPercentage $ testAOV / ctrlAOV,
        _uiCampaignCRChange = nanToNothing $ factorToPercentage $ testCR / ctrlCR,
        _uiCampaignCtrlTreatment =
          UITreatment
            { _uiTreatmentCR = nanToNothing ctrlCR,
              _uiTreatmentAOV =
                nanToZero ctrlAOV
                  & fromFloatDigits
                  & Price
                  & formatPrice moneyFormat,
              _uiTreatmentVariants =
                ctrlTreatmentVariants
                  & map toUITreatmentVariant
                  & L.sortOn (view uiTreatmentSku)
            },
        _uiCampaignTestTreatment =
          UITreatment
            { _uiTreatmentCR = nanToNothing testCR,
              _uiTreatmentAOV =
                nanToZero testAOV
                  & fromFloatDigits
                  & Price
                  & formatPrice moneyFormat,
              _uiTreatmentVariants =
                testTreatmentVariants
                  & map toUITreatmentVariant
                  & L.sortOn (view uiTreatmentSku)
            },
        _uiCampaignUpdatedAt = updatedAt
      }

selectUITreatmentVariants cmpId' treat' = do
  treatment <- all_ (db ^. treatments)
  variant <- all_ (db ^. productVariants)
  guard_ (_treatmentProductVariantId treatment `references_` variant)
  guard_ (treatment ^. treatmentCampaignId ==. val_ cmpId')
  guard_ (treatment ^. treatmentKey ==. val_ treat')
  pure variant

selectShopCampaigns domain = do
  shop <- matchShop domain
  filter_ ((==. (shop ^. shopId)) . (^. campaignShopId)) $
    all_ (db ^. campaigns)

selectCampaignInfParams cmpId' = do
  ctrlRevs <- userRevenueArrForTreatment cmpId' 0
  ctrlNils <- nonConvertersForTreatment cmpId' 0
  testRevs <- userRevenueArrForTreatment cmpId' 1
  testNils <- nonConvertersForTreatment cmpId' 1
  pure (ctrlRevs, ctrlNils, testRevs, testNils)

userRevenueArrForTreatment cmpId' treatment' =
  aggregate_ (\(_, rev) -> pgArrayAgg (fromMaybe_ 0 rev)) $ do
    aggregate_ (\(userId, revenue) -> (group_ userId, sum_ revenue)) $ do
      user <- all_ (db ^. users)
      event <- all_ (db ^. checkoutEvents)
      item <- all_ (db ^. checkoutItems)
      variant <- all_ (db ^. productVariants)
      treatment <- all_ (db ^. treatments)
      guard_ (_checkoutEventUserId event `references_` user)
      guard_ (event ^. checkoutEventCampaignId ==. val_ cmpId')
      guard_ (treatment ^. treatmentCampaignId ==. val_ cmpId')
      guard_ (_checkoutItemCheckoutEventId item `references_` event)
      guard_ (item ^. checkoutItemSvid ==. variant ^. productVariantVariantId)
      guard_ (_treatmentProductVariantId treatment `references_` variant)
      guard_ (treatment ^. treatmentKey ==. treatment')
      let quantity = cast_ (item ^. checkoutItemQuantity) double
          price = cast_ (variant ^. productVariantPrice) double
          revenue = quantity * price
      pure (user ^. userId, revenue)

nonConvertersForTreatment cmpId' treatment' = aggregate_ (const countAll_)
  $ filter_ (\(_, evCount) -> evCount ==. 0)
  $ aggregate_ (\(userId, eventId) -> (group_ userId, as_ @Int (count_ eventId)))
  $ do
    user <- all_ (db ^. users)
    experiment <- all_ (db ^. userExperiments)
    guard_ (_userExperimentUserId experiment `references_` user)
    guard_ (experiment ^. userExperimentTreatment ==. val_ treatment')
    guard_ (experiment ^. userExperimentCampaignId ==. val_ cmpId')
    event <-
      leftJoin_
        (all_ (db ^. checkoutEvents))
        (\ce -> _checkoutEventUserId ce `references_` user)
    pure (user ^. userId, event ^. checkoutEventId)

validateSessionId' :: Connection -> SessionId -> IO (Maybe ShopDomain)
validateSessionId' conn sessionId' =
  runBeamPostgres conn
    $ runSelectReturningOne
    $ select
    $ do
      session <- all_ (db ^. sessions)
      shop <- all_ (db ^. shops)
      guard_ (_sessionShopId session `references_` shop)
      guard_ (session ^. sessionId ==. val_ sessionId')
      pure $ shop ^. shopDomain

readStatsCache :: Connection -> CampaignId -> IO (Maybe UICampaign)
readStatsCache conn cmpId = do
  now <- Clock.getCurrentTime
  result <-
    runBeamPostgres conn
      $ runSelectReturningOne
      $ select
      $ all_ (db ^. statsCaches)
        & filter_ (view statsCacheCampaignId >>> (==. val_ cmpId))
  pure $ case result of
    Nothing -> Nothing
    Just cache ->
      let (PgJSONB json) = cache ^. statsCachePayload
          campaign = (Aeson.parseMaybe Aeson.parseJSON) json & fromJust :: UICampaign
          campaignEnded = campaign ^. uiCampaignEnd & isJust
          cacheValid =
            Clock.diffUTCTime now (campaign ^. uiCampaignUpdatedAt)
              & (<= 60 * 60) -- 60min
       in if campaignEnded || cacheValid
            then Just campaign
            else Nothing

upsertStatsCache :: Connection -> UICampaign -> IO ()
upsertStatsCache conn campaign =
  runBeamPostgres conn
    $ runInsert
    $ PG.insert
      (db ^. statsCaches)
      ( insertExpressions
          [ StatsCache
              { _statsCacheId = pgGenUUID_,
                _statsCacheCampaignId = val_ (CampaignKey (campaign ^. uiCampaignId)),
                _statsCachePayload = val_ (PgJSONB (Aeson.toJSON campaign))
              }
          ]
      )
      ( PG.onConflict
          (PG.conflictingConstraint "stats_cache_campaign_id_key")
          (PG.onConflictUpdateInstead id)
      )
