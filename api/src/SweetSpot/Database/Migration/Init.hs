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
      { _usrId :: Columnar f UserId,
        _usrCreated :: Columnar f UTCTime
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
  primaryKey = UserKey . _usrId

User (LensFor usrId) (LensFor usrCreated) = tableLenses

-- | ---------------------------------------------------------------------------
-- | Campaign
-- | ---------------------------------------------------------------------------
data CampaignT f
  = Campaign
      { _cmpId :: Columnar f CampaignId,
        _cmpShopId :: PrimaryKey ShopT f,
        _cmpName :: Columnar f Text,
        _cmpStart :: Columnar f (Maybe UTCTime),
        _cmpEnd :: Columnar f (Maybe UTCTime)
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
  primaryKey = CampaignKey . _cmpId

Campaign (LensFor cmpId) (ShopKey (LensFor cmpShopId)) (LensFor cmpName) (LensFor cmpStart) (LensFor cmpEnd) =
  tableLenses

-- | ---------------------------------------------------------------------------
-- | ProductVariant
-- | ---------------------------------------------------------------------------
data ProductVariantT f
  = ProductVariant
      { _pvId :: Columnar f PVariantId,
        _pvShopId :: PrimaryKey ShopT f,
        _pvTitle :: Columnar f Text,
        _pvSku :: Columnar f Sku,
        _pvProductId :: Columnar f Pid,
        _pvVariantId :: Columnar f Svid,
        _pvPrice :: Columnar f Price,
        _pvCurrency :: Columnar f Text
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
  primaryKey = PVariantKey . _pvId

ProductVariant (LensFor pvId) (ShopKey (LensFor pvShopId)) (LensFor pvTitle) (LensFor pvSku) (LensFor pvProductId) (LensFor pvVariantId) (LensFor pvPrice) (LensFor pvCurrency) =
  tableLenses

-- | ---------------------------------------------------------------------------
-- | Treatment
-- | ---------------------------------------------------------------------------
data TreatmentT f
  = Treatment
      { _trCmpId :: PrimaryKey CampaignT f,
        _trTreatment :: Columnar f Int,
        _trProductVariantId :: PrimaryKey ProductVariantT f
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
  primaryKey = PriceVariantKey <$> _trCmpId <*> _trProductVariantId

Treatment (CampaignKey (LensFor trCmpId)) (LensFor trTreatment) (PVariantKey (LensFor trProductVariantId)) =
  tableLenses

-- | ---------------------------------------------------------------------------
-- | UserExperiment
-- | ---------------------------------------------------------------------------
data UserExperimentT f
  = UserExperiment
      { _ueUserId :: PrimaryKey UserT f,
        _ueCmpId :: PrimaryKey CampaignT f,
        _ueTreatment :: Columnar f Int
      }
  deriving (Generic, Beamable)

type UserExperiment = UserExperimentT Identity

type UserExperimentKey = PrimaryKey UserExperimentT Identity

instance Table UserExperimentT where
  data PrimaryKey UserExperimentT f
    = UserExperimentKey (PrimaryKey UserT f) (PrimaryKey CampaignT f)
    deriving (Generic, Beamable)
  primaryKey = UserExperimentKey <$> _ueUserId <*> _ueCmpId

UserExperiment (UserKey (LensFor ueUserId)) (CampaignKey (LensFor ueCmpId)) (LensFor ueTreatment) =
  tableLenses

-- | ---------------------------------------------------------------------------
-- | CheckoutEvent
-- | ---------------------------------------------------------------------------
data CheckoutEventT f
  = CheckoutEvent
      { _cevId :: Columnar f EventId,
        _cevCreated :: Columnar f UTCTime,
        _cevCmpId :: PrimaryKey CampaignT f,
        _cevOrderId :: Columnar f OrderId,
        _cevShopId :: PrimaryKey ShopT f,
        _cevUserId :: PrimaryKey UserT f
      }
  deriving (Generic, Beamable)

type CheckoutEvent = CheckoutEventT Identity

type CheckoutEventKey = PrimaryKey CheckoutEventT Identity

instance Table CheckoutEventT where
  data PrimaryKey CheckoutEventT f
    = CheckoutEventKey (Columnar f EventId)
    deriving (Generic, Beamable)
  primaryKey = CheckoutEventKey . _cevId

CheckoutEvent (LensFor cevId) (LensFor cevCreated) (CampaignKey (LensFor cevCmpId)) (LensFor cevOrderId) (ShopKey (LensFor cevShopId)) (UserKey (LensFor cevUserId)) =
  tableLenses

-- | ---------------------------------------------------------------------------
-- | CheckoutItem
-- | ---------------------------------------------------------------------------
data CheckoutItemT f
  = CheckoutItem
      { _ciId :: Columnar f UUID,
        _ciCheckoutEventId :: PrimaryKey CheckoutEventT f,
        _ciQuantity :: Columnar f Int,
        _ciSvid :: Columnar f Svid
      }
  deriving (Generic, Beamable)

type CheckoutItem = CheckoutItemT Identity

type CheckoutItemKey = PrimaryKey CheckoutItemT Identity

instance Table CheckoutItemT where
  data PrimaryKey CheckoutItemT f
    = CheckoutItemKey (Columnar f UUID)
    deriving (Generic, Beamable)
  primaryKey = CheckoutItemKey . _ciId

CheckoutItem (LensFor ciId) (CheckoutEventKey (LensFor ciCheckoutEventId)) (LensFor ciQuantity) (LensFor ciSvid) =
  tableLenses

-- | ---------------------------------------------------------------------------
-- | Event
-- | ---------------------------------------------------------------------------
data EventT f
  = Event
      { _evId :: Columnar f EventId,
        _evPayload :: Columnar f (PgJSONB Value)
      }
  deriving (Generic, Beamable)

type Event = EventT Identity

type EventKey = PrimaryKey EventT Identity

instance Table EventT where
  data PrimaryKey EventT f
    = EventKey (Columnar f EventId)
    deriving (Generic, Beamable)
  primaryKey = EventKey . _evId

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
        _cryptoExtension :: f (PgExtensionEntity PgCrypto)
      }
  deriving (Generic)

instance Database Postgres SweetSpotDb

SweetSpotDb (TableLens shops) (TableLens installNonces) (TableLens users) (TableLens campaigns) (TableLens productVariants) (TableLens treatments) (TableLens userExperiments) (TableLens checkoutEvents) (TableLens checkoutItems) (TableLens events) (TableLens sessions) (TableLens userCartTokens) (TableLens actionRequests) (TableLens cryptoExtension) =
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
          _installNonce = field "nonce" (DataType pgUuidType) notNull
        }
    <*> createTable
      "users"
      User
        { _usrId = field "id" (DataType pgTextType),
          _usrCreated = field "created" ts notNull
        }
    <*> createTable
      "campaigns"
      Campaign
        { _cmpId = field "id" (DataType pgUuidType) notNull,
          _cmpShopId = ShopKey (field "shop_id" (DataType pgUuidType)),
          _cmpName = field "campaign_name" text notNull,
          _cmpStart = field "start_date" (maybeType ts),
          _cmpEnd = field "end_date" (maybeType ts)
        }
    <*> createTable
      "product_variants"
      ProductVariant
        { _pvId = field "id" (DataType pgUuidType) notNull,
          _pvShopId = ShopKey (field "shop_id" (DataType pgUuidType)),
          _pvTitle = field "title" text notNull,
          _pvSku = field "sku" (DataType pgTextType) notNull,
          _pvProductId = field "shopify_product_id" (DataType pgTextType) notNull,
          _pvVariantId = field "shopify_variant_id" (DataType pgTextType) notNull,
          _pvPrice = field "price" priceType notNull,
          _pvCurrency = field "currency" text notNull
        }
    <*> createTable
      "treatments"
      Treatment
        { _trCmpId = CampaignKey (field "campaign_id" (DataType pgUuidType) notNull),
          _trTreatment = field "treatment" int notNull,
          _trProductVariantId = PVariantKey (field "product_variant_id" (DataType pgUuidType) notNull)
        }
    <*> createTable
      "user_experiments"
      UserExperiment
        { _ueUserId = UserKey (field "user_id" (DataType pgTextType) notNull),
          _ueCmpId = CampaignKey (field "campaign_id" (DataType pgUuidType) notNull),
          _ueTreatment = field "treatment" int notNull
        }
    <*> createTable
      "checkout_events"
      CheckoutEvent
        { _cevId = field "id" (DataType pgUuidType) notNull,
          _cevCreated = field "created" ts notNull,
          _cevCmpId = CampaignKey (field "campaign_id" (DataType pgUuidType)),
          _cevOrderId = field "order_id" (DataType pgTextType) notNull,
          _cevShopId = ShopKey (field "shop_id" (DataType pgUuidType)),
          _cevUserId = UserKey (field "user_id" (DataType pgTextType))
        }
    <*> createTable
      "checkout_items"
      CheckoutItem
        { _ciId = field "id" (DataType pgUuidType) notNull,
          _ciCheckoutEventId = CheckoutEventKey (field "checkout_event_id" (DataType pgUuidType)),
          _ciQuantity = field "quantity" int notNull,
          _ciSvid = field "shopify_variant_id" (DataType pgTextType) notNull
        }
    <*> createTable
      "events"
      Event
        { _evId = field "id" (DataType pgUuidType) notNull,
          _evPayload = field "payload" jsonb notNull
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
    <*> pgCreateExtension
