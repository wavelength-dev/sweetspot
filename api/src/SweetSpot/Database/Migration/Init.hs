{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}

module SweetSpot.Database.Migration.Init where

import Data.Aeson (Value)
import Data.Time (UTCTime)
import Data.UUID.Types (UUID)
import Database.Beam
import Database.Beam.Backend.SQL.SQL92
  ( numericType,
    timestampType,
  )
import Database.Beam.Migrate
import Database.Beam.Postgres
import Database.Beam.Postgres.PgCrypto
  ( PgCrypto,
  )
import Database.Beam.Postgres.Syntax
  ( pgTextType,
    pgUuidType,
  )
import RIO
import SweetSpot.Data.Common

-- | ---------------------------------------------------------------------------
-- | Shop
-- | ---------------------------------------------------------------------------
data ShopT f
  = Shop
      { _shopId :: Columnar f ShopId,
        _shopCreated :: Columnar f UTCTime,
        _shopDomain :: Columnar f ShopDomain,
        _shopOAuthToken :: Columnar f Text,
        _shopEmail :: Columnar f Text,
        _shopCountryCode :: Columnar f Text,
        _shopCurrency :: Columnar f Text,
        _shopMoneyFormat :: Columnar f MoneyFormat
      }
  deriving (Generic, Beamable)

type Shop = ShopT Identity

type ShopKey = PrimaryKey ShopT Identity

deriving instance Show Shop

deriving instance Show ShopKey

instance Table ShopT where
  data PrimaryKey ShopT f
    = ShopKey (Columnar f ShopId)
    deriving (Generic, Beamable)
  primaryKey = ShopKey . _shopId

Shop (LensFor shopId) (LensFor shopCreated) (LensFor shopDomain) (LensFor shopOAuthToken) (LensFor shopEmail) (LensFor shopCountryCode) (LensFor shopCurrency) (LensFor shopMoneyFormat) =
  tableLenses

-- | ---------------------------------------------------------------------------
-- | InstallNonce
-- | ---------------------------------------------------------------------------
data InstallNonceT f
  = InstallNonce
      { _installShopDomain :: Columnar f ShopDomain,
        _installNonce :: Columnar f Nonce
      }
  deriving (Generic, Beamable)

type InstallNonce = InstallNonceT Identity

type InstallNonceKey = PrimaryKey InstallNonceT Identity

instance Table InstallNonceT where
  data PrimaryKey InstallNonceT f
    = InstallNonceKey (Columnar f ShopDomain)
    deriving (Generic, Beamable)
  primaryKey = InstallNonceKey . _installShopDomain

InstallNonce (LensFor installShopDomain) (LensFor installNonce) = tableLenses

-- | ---------------------------------------------------------------------------
-- | User
-- | ---------------------------------------------------------------------------
data UserT f
  = User
      { _userId :: Columnar f UserId,
        _userCreated :: Columnar f UTCTime
      }
  deriving (Generic, Beamable)

type User = UserT Identity

type UserKey = PrimaryKey UserT Identity

deriving instance Show User

deriving instance Eq User

instance Table UserT where
  data PrimaryKey UserT f
    = UserKey (Columnar f UserId)
    deriving (Generic, Beamable)
  primaryKey = UserKey . _userId

User (LensFor userId) (LensFor userCreated) = tableLenses

-- | ---------------------------------------------------------------------------
-- | Campaign
-- | ---------------------------------------------------------------------------
data CampaignT f
  = Campaign
      { _campaignId :: Columnar f CampaignId,
        _campaignShopId :: PrimaryKey ShopT f,
        _campaignName :: Columnar f Text,
        _campaignStart :: Columnar f (Maybe UTCTime),
        _campaignEnd :: Columnar f (Maybe UTCTime)
      }
  deriving (Generic, Beamable)

type Campaign = CampaignT Identity

type CampaignKey = PrimaryKey CampaignT Identity

deriving instance Show Campaign

deriving instance Show CampaignKey

instance Table CampaignT where
  data PrimaryKey CampaignT f
    = CampaignKey (Columnar f CampaignId)
    deriving (Generic, Beamable)
  primaryKey = CampaignKey . _campaignId

Campaign (LensFor campaignId) (ShopKey (LensFor campaignShopId)) (LensFor campaignName) (LensFor campaignStart) (LensFor campaignEnd) =
  tableLenses

-- | ---------------------------------------------------------------------------
-- | ProductVariant
-- | ---------------------------------------------------------------------------
data ProductVariantT f
  = ProductVariant
      { _productVariantId :: Columnar f PVariantId,
        _productVariantShopId :: PrimaryKey ShopT f,
        _productVariantTitle :: Columnar f Text,
        _productVariantSku :: Columnar f Sku,
        _productVariantProductId :: Columnar f Pid,
        _productVariantVariantId :: Columnar f Svid,
        _productVariantPrice :: Columnar f Price,
        _productVariantCurrency :: Columnar f Text
      }
  deriving (Generic, Beamable)

type ProductVariant = ProductVariantT Identity

type PVariantKey = PrimaryKey ProductVariantT Identity

deriving instance Show ProductVariant

deriving instance Show PVariantKey

instance Table ProductVariantT where
  data PrimaryKey ProductVariantT f
    = PVariantKey (Columnar f PVariantId)
    deriving (Generic, Beamable)
  primaryKey = PVariantKey . _productVariantId

ProductVariant (LensFor productVariantId) (ShopKey (LensFor productVariantShopId)) (LensFor productVariantTitle) (LensFor productVariantSku) (LensFor productVariantProductId) (LensFor productVariantVariantId) (LensFor productVariantPrice) (LensFor productVariantCurrency) =
  tableLenses

-- | ---------------------------------------------------------------------------
-- | Treatment
-- | ---------------------------------------------------------------------------
data TreatmentT f
  = Treatment
      { _treatmentCampaignId :: PrimaryKey CampaignT f,
        _treatmentKey :: Columnar f Int,
        _treatmentProductVariantId :: PrimaryKey ProductVariantT f
      }
  deriving (Generic, Beamable)

type Treatment = TreatmentT Identity

type TreatmentKey = PrimaryKey TreatmentT Identity

deriving instance Show Treatment

deriving instance Show TreatmentKey

instance Table TreatmentT where
  data PrimaryKey TreatmentT f
    = PriceVariantKey (PrimaryKey CampaignT f) (PrimaryKey ProductVariantT f)
    deriving (Generic, Beamable)
  primaryKey = PriceVariantKey <$> _treatmentCampaignId <*> _treatmentProductVariantId

Treatment (CampaignKey (LensFor treatmentCampaignId)) (LensFor treatmentKey) (PVariantKey (LensFor treatmentProductVariantId)) =
  tableLenses

-- | ---------------------------------------------------------------------------
-- | UserExperiment
-- | ---------------------------------------------------------------------------
data UserExperimentT f
  = UserExperiment
      { _userExperimentUserId :: PrimaryKey UserT f,
        _userExperimentCampaignId :: PrimaryKey CampaignT f,
        _userExperimentTreatment :: Columnar f Int
      }
  deriving (Generic, Beamable)

type UserExperiment = UserExperimentT Identity

type UserExperimentKey = PrimaryKey UserExperimentT Identity

instance Table UserExperimentT where
  data PrimaryKey UserExperimentT f
    = UserExperimentKey (PrimaryKey UserT f) (PrimaryKey CampaignT f)
    deriving (Generic, Beamable)
  primaryKey = UserExperimentKey <$> _userExperimentUserId <*> _userExperimentCampaignId

UserExperiment (UserKey (LensFor userExperimentUserId)) (CampaignKey (LensFor userExperimentCampaignId)) (LensFor userExperimentTreatment) =
  tableLenses

-- | ---------------------------------------------------------------------------
-- | CheckoutEvent
-- | ---------------------------------------------------------------------------
data CheckoutEventT f
  = CheckoutEvent
      { _checkoutEventId :: Columnar f EventId,
        _checkoutEventCreated :: Columnar f UTCTime,
        _checkoutEventCampaignId :: PrimaryKey CampaignT f,
        _checkoutEventOrderId :: Columnar f OrderId,
        _checkoutEventShopId :: PrimaryKey ShopT f,
        _checkoutEventUserId :: PrimaryKey UserT f
      }
  deriving (Generic, Beamable)

type CheckoutEvent = CheckoutEventT Identity

type CheckoutEventKey = PrimaryKey CheckoutEventT Identity

instance Table CheckoutEventT where
  data PrimaryKey CheckoutEventT f
    = CheckoutEventKey (Columnar f EventId)
    deriving (Generic, Beamable)
  primaryKey = CheckoutEventKey . _checkoutEventId

CheckoutEvent (LensFor checkoutEventId) (LensFor checkoutEventCreated) (CampaignKey (LensFor checkoutEventCampaignId)) (LensFor checkoutEventOrderId) (ShopKey (LensFor checkoutEventShopId)) (UserKey (LensFor checkoutEventUserId)) =
  tableLenses

-- | ---------------------------------------------------------------------------
-- | CheckoutItem
-- | ---------------------------------------------------------------------------
data CheckoutItemT f
  = CheckoutItem
      { _checkoutItemId :: Columnar f UUID,
        _checkoutItemCheckoutEventId :: PrimaryKey CheckoutEventT f,
        _checkoutItemQuantity :: Columnar f Int,
        _checkoutItemSvid :: Columnar f Svid
      }
  deriving (Generic, Beamable)

type CheckoutItem = CheckoutItemT Identity

type CheckoutItemKey = PrimaryKey CheckoutItemT Identity

instance Table CheckoutItemT where
  data PrimaryKey CheckoutItemT f
    = CheckoutItemKey (Columnar f UUID)
    deriving (Generic, Beamable)
  primaryKey = CheckoutItemKey . _checkoutItemId

CheckoutItem (LensFor checkoutItemId) (CheckoutEventKey (LensFor checkoutItemCheckoutEventId)) (LensFor checkoutItemQuantity) (LensFor checkoutItemSvid) =
  tableLenses

-- | ---------------------------------------------------------------------------
-- | Event
-- | ---------------------------------------------------------------------------
data EventT f
  = Event
      { _eventId :: Columnar f EventId,
        _eventPayload :: Columnar f (PgJSONB Value)
      }
  deriving (Generic, Beamable)

type Event = EventT Identity

type EventKey = PrimaryKey EventT Identity

instance Table EventT where
  data PrimaryKey EventT f
    = EventKey (Columnar f EventId)
    deriving (Generic, Beamable)
  primaryKey = EventKey . _eventId

-- | ---------------------------------------------------------------------------
-- | Session
-- | ---------------------------------------------------------------------------
data SessionT f
  = Session
      { _sessionId :: Columnar f SessionId,
        _sessionShopId :: PrimaryKey ShopT f
      }
  deriving (Generic, Beamable)

type Session = SessionT Identity

type SessionKey = PrimaryKey SessionT Identity

deriving instance Show Session

deriving instance Show SessionKey

instance Table SessionT where
  data PrimaryKey SessionT f
    = SessionKey (Columnar f SessionId)
    deriving (Generic, Beamable)
  primaryKey = SessionKey . _sessionId

Session (LensFor sessionId) (ShopKey (LensFor sessionShopId)) = tableLenses

sessionIdType :: DataType Postgres SessionId
sessionIdType = DataType pgTextType

-- | ---------------------------------------------------------------------------
-- | UserCartToken
-- | ---------------------------------------------------------------------------
data UserCartTokenT f
  = UserCartToken
      { _cartTokenId :: Columnar f CartToken,
        _cartTokenUser :: PrimaryKey UserT f
      }
  deriving (Generic, Beamable)

type UserCartToken = UserCartTokenT Identity

type UserCartTokenKey = PrimaryKey UserCartTokenT Identity

instance Table UserCartTokenT where
  data PrimaryKey UserCartTokenT f
    = UserCartTokenKey (Columnar f CartToken)
    deriving (Generic, Beamable)
  primaryKey = UserCartTokenKey . _cartTokenId

UserCartToken (LensFor cartTokenId) (UserKey (LensFor cartTokenUser)) = tableLenses

-- | ---------------------------------------------------------------------------
-- | ActionRequest
-- | ---------------------------------------------------------------------------
data ActionRequestT f
  = ActionRequest
      { _actionRequestId :: Columnar f UUID,
        _actionRequestShopId :: PrimaryKey ShopT f,
        _actionRequestType :: Columnar f ActionRequestType,
        _actionRequestPayload :: Columnar f (PgJSONB Value)
      }
  deriving (Generic, Beamable)

type ActionRequest = ActionRequestT Identity

type ActionRequestKey = PrimaryKey ActionRequestT Identity

instance Table ActionRequestT where
  data PrimaryKey ActionRequestT f
    = ActionRequestKey (Columnar f UUID)
    deriving (Generic, Beamable)
  primaryKey = ActionRequestKey . _actionRequestId

ActionRequest (LensFor actionRequestId) (ShopKey (LensFor actionRequestShopId)) (LensFor actionRequestType) (LensFor actionRequestPayload) = tableLenses

-- | ---------------------------------------------------------------------------
-- | AppCharge
-- | ---------------------------------------------------------------------------
data AppChargeT f
  = AppCharge
      { _appChargeId :: Columnar f UUID,
        _appChargeShopifyId :: Columnar f Text,
        _appChargeStatus :: Columnar f AppChargeStatus,
        _appChargeShopId :: PrimaryKey ShopT f,
        _appChargeName :: Columnar f Text,
        _appChargePrice :: Columnar f Text,
        _appChargeReturnUrl :: Columnar f Text,
        _appChargeConfirmationUrl :: Columnar f Text
      }
  deriving (Generic, Beamable)

type AppCharge = AppChargeT Identity

type AppChargeKey = PrimaryKey AppChargeT Identity

instance Table AppChargeT where
  data PrimaryKey AppChargeT f
    = AppChargeKey (Columnar f UUID)
    deriving (Generic, Beamable)
  primaryKey = AppChargeKey . _appChargeId

AppCharge (LensFor appChargeId) (LensFor appChargeShopifyId) (LensFor appChargeStatus) (ShopKey (LensFor appChargeShopId)) (LensFor appChargeName) (LensFor appChargePrice) (LensFor appChargeReturnUrl) (LensFor appChargeConfirmationUrl) = tableLenses

-- | ---------------------------------------------------------------------------
-- | UnaccountedOrder
-- | ---------------------------------------------------------------------------
data UnaccountedOrderT f
  = UnaccountedOrder
      { _unaccountedOrderId :: Columnar f UUID,
        _unaccountedOrderShopId :: PrimaryKey ShopT f,
        _unaccountedOrderPayload :: Columnar f (PgJSONB Value)
      }
  deriving (Generic, Beamable)

type UnaccountedOrder = UnaccountedOrderT Identity

type UnaccountedOrderKey = PrimaryKey UnaccountedOrderT Identity

instance Table UnaccountedOrderT where
  data PrimaryKey UnaccountedOrderT f
    = UnaccountedOrderKey (Columnar f UUID)
    deriving (Generic, Beamable)
  primaryKey = UnaccountedOrderKey . _unaccountedOrderId

UnaccountedOrder (LensFor unaccountedOrderId) (ShopKey (LensFor unaccountedOrderShopId)) (LensFor unaccountedOrderPayload) = tableLenses

-- | ---------------------------------------------------------------------------
-- | Database
-- | ---------------------------------------------------------------------------
data SweetSpotDb f
  = SweetSpotDb
      { _shops :: f (TableEntity ShopT),
        _installNonces :: f (TableEntity InstallNonceT),
        _users :: f (TableEntity UserT),
        _campaigns :: f (TableEntity CampaignT),
        _productVariants :: f (TableEntity ProductVariantT),
        _treatments :: f (TableEntity TreatmentT),
        _userExperiments :: f (TableEntity UserExperimentT),
        _checkoutEvents :: f (TableEntity CheckoutEventT),
        _checkoutItems :: f (TableEntity CheckoutItemT),
        _events :: f (TableEntity EventT),
        _sessions :: f (TableEntity SessionT),
        _userCartTokens :: f (TableEntity UserCartTokenT),
        _actionRequests :: f (TableEntity ActionRequestT),
        _appCharges :: f (TableEntity AppChargeT),
        _unaccountedOrders :: f (TableEntity UnaccountedOrderT),
        _cryptoExtension :: f (PgExtensionEntity PgCrypto)
      }
  deriving (Generic)

instance Database Postgres SweetSpotDb

SweetSpotDb (TableLens shops) (TableLens installNonces) (TableLens users) (TableLens campaigns) (TableLens productVariants) (TableLens treatments) (TableLens userExperiments) (TableLens checkoutEvents) (TableLens checkoutItems) (TableLens events) (TableLens sessions) (TableLens userCartTokens) (TableLens actionRequests) (TableLens appCharges) (TableLens unaccountedOrders) (TableLens cryptoExtension) =
  dbLenses

-- | ---------------------------------------------------------------------------
-- | Migration types
-- | ---------------------------------------------------------------------------
priceType :: DataType Postgres Price
priceType = DataType (numericType (Just (12, Just 2)))

ts :: DataType Postgres UTCTime
ts = DataType (timestampType Nothing True)

-- | ---------------------------------------------------------------------------
-- | Migration
-- | ---------------------------------------------------------------------------
migration ::
  () ->
  Migration Postgres (CheckedDatabaseSettings Postgres SweetSpotDb)
migration () =
  SweetSpotDb
    <$> createTable
      "shops"
      Shop
        { _shopId = field "id" (DataType pgUuidType) notNull,
          _shopCreated = field "created" ts notNull,
          _shopDomain = field "shop_domain" (DataType pgTextType) notNull,
          _shopOAuthToken = field "oauth_token" text notNull,
          _shopEmail = field "email" text notNull,
          _shopCountryCode = field "country_code" text notNull,
          _shopCurrency = field "currency" text notNull,
          _shopMoneyFormat = field "money_format" (DataType pgTextType) notNull
        }
    <*> createTable
      "install_nonces"
      InstallNonce
        { _installShopDomain = field "shop_domain" (DataType pgTextType) notNull,
          _installNonce = field "nonce" (DataType pgTextType) notNull
        }
    <*> createTable
      "users"
      User
        { _userId = field "id" (DataType pgTextType),
          _userCreated = field "created" ts notNull
        }
    <*> createTable
      "campaigns"
      Campaign
        { _campaignId = field "id" (DataType pgUuidType) notNull,
          _campaignShopId = ShopKey (field "shop_id" (DataType pgUuidType)),
          _campaignName = field "campaign_name" text notNull,
          _campaignStart = field "start_date" (maybeType ts),
          _campaignEnd = field "end_date" (maybeType ts)
        }
    <*> createTable
      "product_variants"
      ProductVariant
        { _productVariantId = field "id" (DataType pgUuidType) notNull,
          _productVariantShopId = ShopKey (field "shop_id" (DataType pgUuidType)),
          _productVariantTitle = field "title" text notNull,
          _productVariantSku = field "sku" (DataType pgTextType) notNull,
          _productVariantProductId = field "shopify_product_id" (DataType pgTextType) notNull,
          _productVariantVariantId = field "shopify_variant_id" (DataType pgTextType) notNull,
          _productVariantPrice = field "price" priceType notNull,
          _productVariantCurrency = field "currency" text notNull
        }
    <*> createTable
      "treatments"
      Treatment
        { _treatmentCampaignId = CampaignKey (field "campaign_id" (DataType pgUuidType) notNull),
          _treatmentKey = field "treatment" int notNull,
          _treatmentProductVariantId = PVariantKey (field "product_variant_id" (DataType pgUuidType) notNull)
        }
    <*> createTable
      "user_experiments"
      UserExperiment
        { _userExperimentUserId = UserKey (field "user_id" (DataType pgTextType) notNull),
          _userExperimentCampaignId = CampaignKey (field "campaign_id" (DataType pgUuidType) notNull),
          _userExperimentTreatment = field "treatment" int notNull
        }
    <*> createTable
      "checkout_events"
      CheckoutEvent
        { _checkoutEventId = field "id" (DataType pgUuidType) notNull,
          _checkoutEventCreated = field "created" ts notNull,
          _checkoutEventCampaignId = CampaignKey (field "campaign_id" (DataType pgUuidType)),
          _checkoutEventOrderId = field "order_id" (DataType pgTextType) notNull,
          _checkoutEventShopId = ShopKey (field "shop_id" (DataType pgUuidType)),
          _checkoutEventUserId = UserKey (field "user_id" (DataType pgTextType))
        }
    <*> createTable
      "checkout_items"
      CheckoutItem
        { _checkoutItemId = field "id" (DataType pgUuidType) notNull,
          _checkoutItemCheckoutEventId = CheckoutEventKey (field "checkout_event_id" (DataType pgUuidType)),
          _checkoutItemQuantity = field "quantity" int notNull,
          _checkoutItemSvid = field "shopify_variant_id" (DataType pgTextType) notNull
        }
    <*> createTable
      "events"
      Event
        { _eventId = field "id" (DataType pgUuidType) notNull,
          _eventPayload = field "payload" jsonb notNull
        }
    <*> createTable
      "sessions"
      Session
        { _sessionId = field "id" (DataType pgTextType),
          _sessionShopId = ShopKey (field "shop_id" (DataType pgUuidType))
        }
    <*> createTable
      "user_cart_tokens"
      UserCartToken
        { _cartTokenId = field "cart_token" (DataType pgTextType) notNull,
          _cartTokenUser = UserKey (field "user_id" (DataType pgTextType))
        }
    <*> createTable
      "action_requests"
      ActionRequest
        { _actionRequestId = field "id" (DataType pgUuidType) notNull,
          _actionRequestShopId = ShopKey (field "shop_id" (DataType pgUuidType)),
          _actionRequestType = field "request_type" (DataType pgTextType) notNull,
          _actionRequestPayload = field "payload" jsonb notNull
        }
    <*> createTable
      "app_charges"
      AppCharge
        { _appChargeId = field "id" (DataType pgUuidType) notNull,
          _appChargeStatus = field "status" (DataType pgTextType) notNull,
          _appChargeShopifyId = field "shopify_charge_id" text notNull,
          _appChargeShopId = ShopKey (field "shop_id" (DataType pgUuidType)),
          _appChargeName = field "name" text notNull,
          _appChargePrice = field "price" text notNull,
          _appChargeReturnUrl = field "return_url" text notNull,
          _appChargeConfirmationUrl = field "confirmation_url" text notNull
        }
    <*> createTable
      "unaccounted_orders"
      UnaccountedOrder
        { _unaccountedOrderId = field "id" (DataType pgUuidType) notNull,
          _unaccountedOrderShopId = ShopKey (field "shop_id" (DataType pgUuidType)),
          _unaccountedOrderPayload = field "payload" jsonb notNull
        }
    <*> pgCreateExtension
