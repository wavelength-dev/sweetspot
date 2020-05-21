{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module SweetSpot.Database.Queries.Dashboard
  ( DashboardDB (..),
    InsertExperiment (..),
    validateSessionId',
  )
where

import Control.Lens
import Data.Scientific (fromFloatDigits)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions as BeamExt
import Database.Beam.Postgres
import RIO hiding (Vector, (^.), view)
import RIO.Partial (fromJust)
import RIO.Vector as V hiding (map)
import Statistics.Sample (mean)
import SweetSpot.AppM (AppM)
import SweetSpot.Calc (InfParams (..), runInference)
import SweetSpot.Data.Api hiding (productVariants)
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Util
  ( matchShop,
    selectShopMoneyFormat,
    withConn,
  )
import SweetSpot.Database.Schema
import SweetSpot.Util (formatPrice, nanToZero)

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

instance DashboardDB AppM where
  createCampaign domain cc = withConn $ \conn -> do
    shopId <- unsafeFindShopId conn domain
    [newCmp] <-
      runBeamPostgres conn
        $ BeamExt.runInsertReturningList
        $ insert (db ^. campaigns)
        $ insertExpressions
          [ Campaign
              { _cmpId = campaignId_,
                _cmpShopId = val_ $ ShopKey shopId,
                _cmpName = val_ $ cc ^. createCampaignName,
                _cmpStart = just_ nowUTC_,
                _cmpEnd = val_ $ cc ^. createCampaignEnd
              }
          ]
    pure $ newCmp ^. cmpId

  createExperiment args = withConn $ \conn -> do
    let domain = args ^. insertExperimentShopDomain
    Just shopId <- runBeamPostgres conn
      $ runSelectReturningOne
      $ select
      $ do
        row <- matchShop domain
        pure $ row ^. shopId
    [dbVariant] <-
      runBeamPostgres conn
        $ runInsertReturningList
        $ insert (db ^. productVariants)
        $ insertExpressions
          [ ProductVariant
              { _pvId = productVariant_,
                _pvShopId = val_ $ ShopKey shopId,
                _pvTitle = val_ $ args ^. insertExperimentProductName,
                _pvSku = val_ $ args ^. insertExperimentSku,
                _pvProductId = val_ $ args ^. insertExperimentProductId,
                _pvVariantId = val_ $ args ^. insertExperimentSvid,
                _pvPrice = val_ $ args ^. insertExperimentPrice,
                _pvCurrency = val_ "USD"
              }
          ]
    runBeamPostgres conn
      $ runInsert
      $ insert (db ^. treatments)
      $ insertExpressions
        [ Treatment
            { _trCmpId = val_ $ CampaignKey $ args ^. insertExperimentCampaignId,
              _trTreatment = val_ $ args ^. insertExperimentTreatment,
              _trProductVariantId = val_ $ PVariantKey $ dbVariant ^. pvId
            }
        ]

  getCampaigns domain = withConn $ \conn -> do
    cmps <-
      runBeamPostgres conn
        $ runSelectReturningList
        $ select
        $ selectShopCampaigns domain
    traverse (enhanceCampaign conn domain) cmps

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

enhanceCampaign :: Connection -> ShopDomain -> Campaign -> IO UICampaign
enhanceCampaign conn domain cmp = do
  let cmpId' = cmp ^. cmpId
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
      toUITreatmentVariant (title, sku, price) =
        UITreatmentVariant
          { _uiTreatmentVariantTitle = title,
            _uiTreatmentSku = sku,
            _uiTreatmentVariantPrice = formatPrice moneyFormat price
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
  return
    UICampaign
      { _uiCampaignId = cmp ^. cmpId,
        _uiCampaignName = cmp ^. cmpName,
        _uiCampaignStart = cmp ^. cmpStart,
        _uiCampaignEnd = cmp ^. cmpEnd,
        _uiCampaignLift = infResult,
        _uiCampaignAOVChange = nanToZero $ testAOV / ctrlAOV,
        _uiCampaignCtrlTreatment =
          UITreatment
            { _uiTreatmentCR = nanToZero ctrlCR,
              _uiTreatmentAOV =
                nanToZero ctrlAOV
                  & fromFloatDigits
                  & Price
                  & formatPrice moneyFormat,
              _uiTreatmentVariants =
                map toUITreatmentVariant ctrlTreatmentVariants
            },
        _uiCampaignTestTreatment =
          UITreatment
            { _uiTreatmentCR = nanToZero testCR,
              _uiTreatmentAOV =
                nanToZero testAOV
                  & fromFloatDigits
                  & Price
                  & formatPrice moneyFormat,
              _uiTreatmentVariants =
                map toUITreatmentVariant testTreatmentVariants
            }
      }

selectUITreatmentVariants cmpId' treat' = do
  treatment <- all_ (db ^. treatments)
  variant <- all_ (db ^. productVariants)
  guard_ (_trProductVariantId treatment `references_` variant)
  guard_ (treatment ^. trCmpId ==. val_ cmpId')
  guard_ (treatment ^. trTreatment ==. val_ treat')
  pure (variant ^. pvTitle, variant ^. pvSku, variant ^. pvPrice)

selectShopCampaigns domain = do
  shop <- matchShop domain
  filter_ ((==. (shop ^. shopId)) . (^. cmpShopId)) $
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
      guard_ (_cevUserId event `references_` user)
      guard_ (event ^. cevCmpId ==. val_ cmpId')
      guard_ (_ciCheckoutEventId item `references_` event)
      guard_ (item ^. ciSvid ==. variant ^. pvVariantId)
      guard_ (_trProductVariantId treatment `references_` variant)
      guard_ (treatment ^. trTreatment ==. treatment')
      let quantity = cast_ (item ^. ciQuantity) double
          price = cast_ (variant ^. pvPrice) double
          revenue = quantity * price
      pure (user ^. usrId, revenue)

nonConvertersForTreatment cmpId' treatment' = aggregate_ (const countAll_)
  $ filter_ (\(_, evCount) -> evCount ==. 0)
  $ aggregate_ (\(userId, eventId) -> (group_ userId, as_ @Int (count_ eventId)))
  $ do
    user <- all_ (db ^. users)
    experiment <- all_ (db ^. userExperiments)
    guard_ (_ueUserId experiment `references_` user)
    guard_ (experiment ^. ueTreatment ==. val_ treatment')
    guard_ (experiment ^. ueCmpId ==. val_ cmpId')
    event <-
      leftJoin_
        (all_ (db ^. checkoutEvents))
        (\ce -> _cevUserId ce `references_` user)
    pure (user ^. usrId, event ^. cevId)

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

unsafeFindShopId :: Connection -> ShopDomain -> IO ShopId
unsafeFindShopId conn domain =
  fromJust
    <$> ( runBeamPostgres conn
            $ runSelectReturningOne
            $ select
            $ view shopId <$> matchShop domain
        )
