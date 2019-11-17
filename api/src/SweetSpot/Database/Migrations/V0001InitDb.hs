{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

module SweetSpot.Database.Migrations.V0001InitDb where

import           Data.Aeson                     ( Value )
import           Database.Beam
import           Database.Beam.Backend.SQL.Types
                                                ( SqlSerial )
import           Database.Beam.Backend.SQL.SQL92
                                                ( numericType
                                                , intType
                                                )
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax  ( pgTextType
                                                , pgSerialType
                                                , pgUuidType
                                                )
import           Database.Beam.Postgres.PgCrypto
                                                ( PgCrypto )
import           Database.Beam.Migrate
import           Data.Text                      ( Text )
import           Data.Time                      ( LocalTime )
import           Data.UUID.Types                ( UUID )
import           Data.Vector                    ( Vector )
import           SweetSpot.Data.Common

-- | ---------------------------------------------------------------------------
-- | Shop
-- | ---------------------------------------------------------------------------
data ShopT f
  = Shop
  { _shopId :: Columnar f ShopId
  , _shopCreated :: Columnar f LocalTime
  , _shopShopifyId :: Columnar f Text
  , _shopName :: Columnar f Text
  , _shopClientId :: Columnar f Text
  , _shopOauthToken :: Columnar f Text
  } deriving (Generic, Beamable)

type Shop = ShopT Identity
type ShopKey = PrimaryKey ShopT Identity

deriving instance Show Shop
deriving instance Show ShopKey

instance Table ShopT where
        data PrimaryKey ShopT f
          = ShopKey (Columnar f ShopId) deriving (Generic, Beamable)
        primaryKey = ShopKey . _shopId

Shop (LensFor shopId) (LensFor shopCreated) (LensFor shopifyId) (LensFor name) (LensFor clientId) (LensFor oauthToken)
        = tableLenses

-- | ---------------------------------------------------------------------------
-- | User
-- | ---------------------------------------------------------------------------
data UserT f
  = User
  { _usrId :: Columnar f UserId
  , _usrCreated :: Columnar f LocalTime
  } deriving (Generic, Beamable)

type User = UserT Identity
type UserKey = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User

instance Table UserT where
        data PrimaryKey UserT f
          = UserKey (Columnar f UserId) deriving (Generic, Beamable)
        primaryKey = UserKey . _usrId

User (LensFor usrId) (LensFor usrCreated) = tableLenses

-- | ---------------------------------------------------------------------------
-- | Campaign
-- | ---------------------------------------------------------------------------
data CampaignT f
  = Campaign
  { _cmpId :: Columnar f CampaignId
  , _cmpShopId :: PrimaryKey ShopT f
  , _cmpName :: Columnar f Text
  , _cmpStart :: Columnar f LocalTime
  , _cmpEnd :: Columnar f LocalTime
  } deriving (Generic, Beamable)

type Campaign = CampaignT Identity
type CampaignKey = PrimaryKey CampaignT Identity

deriving instance Show Campaign
deriving instance Show CampaignKey

instance Table CampaignT where
        data PrimaryKey CampaignT f
          = CampaignKey (Columnar f CampaignId) deriving (Generic, Beamable)
        primaryKey = CampaignKey . _cmpId

Campaign (LensFor cmpId) (ShopKey (LensFor cmpShopId)) (LensFor cmpName) (LensFor cmpStart) (LensFor cmpEnd)
        = tableLenses

-- | ---------------------------------------------------------------------------
-- | ProductVariant
-- | ---------------------------------------------------------------------------
data ProductVariantT f
  = ProductVariant
  { _pvId :: Columnar f PVariantId
  , _pvShopId :: PrimaryKey ShopT f
  , _pvTitle :: Columnar f Text
  , _pvSku :: Columnar f Sku
  , _pvProductId :: Columnar f Pid
  , _pvVariantId :: Columnar f Svid
  , _pvPrice :: Columnar f Price
  , _pvCurrency :: Columnar f Text
  } deriving (Generic, Beamable)

type ProductVariant = ProductVariantT Identity
type PVariantKey = PrimaryKey ProductVariantT Identity

deriving instance Show ProductVariant
deriving instance Show PVariantKey

instance Table ProductVariantT where
        data PrimaryKey ProductVariantT f
          = PVariantKey (Columnar f PVariantId) deriving (Generic, Beamable)
        primaryKey = PVariantKey . _pvId

ProductVariant (LensFor pvId) (ShopKey (LensFor pvShopId)) (LensFor pvTitle) (LensFor pvSku) (LensFor pvProductId) (LensFor pvVariantId) (LensFor pvPrice) (LensFor pvCurrency)
        = tableLenses

-- | ---------------------------------------------------------------------------
-- | PriceVariant
-- | ---------------------------------------------------------------------------
data PriceVariantT f
  = PriceVariant
  { _prvCmpId :: PrimaryKey CampaignT f
  , _prvTreatment :: Columnar f Int
  , _prvProductVariantId :: PrimaryKey ProductVariantT f
  } deriving (Generic, Beamable)

type PriceVariant = PriceVariantT Identity
type PriceVariantKey = PrimaryKey PriceVariantT Identity

deriving instance Show PriceVariant
deriving instance Show PriceVariantKey

instance Table PriceVariantT where
        data PrimaryKey PriceVariantT f
          = PriceVariantKey (PrimaryKey CampaignT f) (PrimaryKey ProductVariantT f)
            deriving (Generic, Beamable)
        primaryKey = PriceVariantKey <$> _prvCmpId <*> _prvProductVariantId

PriceVariant (CampaignKey (LensFor pvrCmpId)) (LensFor pvrTreatment) (PVariantKey (LensFor pvrProductVariantId))
        = tableLenses

-- | ---------------------------------------------------------------------------
-- | UserExperiment
-- | ---------------------------------------------------------------------------
data UserExperimentT f
  = UserExperiment
  { _ueUserId :: PrimaryKey UserT f
  , _ueCmpId :: PrimaryKey CampaignT f
  , _ueTreatment :: Columnar f Int
  } deriving (Generic, Beamable)

type UserExperiment = UserExperimentT Identity
type UserExperimentKey = PrimaryKey UserExperimentT Identity

instance Table UserExperimentT where
        data PrimaryKey UserExperimentT f
          = UserExperimentKey (PrimaryKey UserT f) (PrimaryKey CampaignT f)
            deriving (Generic, Beamable)
        primaryKey = UserExperimentKey <$> _ueUserId <*> _ueCmpId

UserExperiment (UserKey (LensFor ueUserId)) (CampaignKey (LensFor ueCmpId)) (LensFor ueTreatment)
        = tableLenses


-- | ---------------------------------------------------------------------------
-- | CheckoutEvent
-- | ---------------------------------------------------------------------------
data CheckoutEventT f
  = CheckoutEvent
  { _cevId :: Columnar f EventId
  , _cevCreated :: Columnar f LocalTime
  , _cevCmpId :: PrimaryKey CampaignT f
  , _cevOrderId :: Columnar f Text
  , _cevShopId :: PrimaryKey ShopT f
  , _cevUserId :: PrimaryKey UserT f
  , _cevItems :: Columnar f (Vector Text)
  } deriving (Generic, Beamable)

type CheckoutEvent = CheckoutEventT Identity
type CheckoutEventKey = PrimaryKey CheckoutEventT Identity

instance Table CheckoutEventT where
        data PrimaryKey CheckoutEventT f
          = CheckoutEventKey (Columnar f EventId) deriving (Generic, Beamable)
        primaryKey = CheckoutEventKey . _cevId

CheckoutEvent (LensFor cevId) (LensFor cevCreated) (CampaignKey (LensFor cevCmpId)) (LensFor cevOrderId) (ShopKey (LensFor cevShopId)) (UserKey (LensFor cevUserId)) (LensFor cevItems)
        = tableLenses

-- | ---------------------------------------------------------------------------
-- | Event
-- | ---------------------------------------------------------------------------
data EventT f
  = Event
  { _evId :: Columnar f EventId
  , _evPayload :: Columnar f (PgJSONB Value)
  } deriving (Generic, Beamable)

type Event = EventT Identity
type EventKey = PrimaryKey EventT Identity

instance Table EventT where
        data PrimaryKey EventT f
          = EventKey (Columnar f EventId) deriving (Generic, Beamable)
        primaryKey = EventKey . _evId

-- | ---------------------------------------------------------------------------
-- | Database
-- | ---------------------------------------------------------------------------
data SweetSpotDb f = SweetSpotDb
  { _shops :: f (TableEntity ShopT)
  , _users :: f (TableEntity UserT)
  , _campaigns :: f (TableEntity CampaignT)
  , _productVariants :: f (TableEntity ProductVariantT)
  , _priceVariants :: f (TableEntity PriceVariantT)
  , _userExperiments :: f (TableEntity UserExperimentT)
  , _checkoutEvents :: f (TableEntity CheckoutEventT)
  , _events :: f (TableEntity EventT)
  , _cryptoExtension :: f (PgExtensionEntity PgCrypto)
  } deriving (Generic)

instance Database Postgres SweetSpotDb

SweetSpotDb (TableLens shops) (TableLens users) (TableLens campaigns) (TableLens productVariants) (TableLens priceVariants) (TableLens userExperiments) (TableLens checkoutEvents) (TableLens events) (TableLens cryptoExtension)
        = dbLenses

-- | ---------------------------------------------------------------------------
-- | Types
-- | ---------------------------------------------------------------------------
pricePrecision :: Maybe (Word, Maybe Word)
pricePrecision = Just (12, Just 2)

campaignIdType :: DataType Postgres CampaignId
campaignIdType = DataType pgTextType

shopIdType :: DataType Postgres ShopId
shopIdType = DataType pgUuidType

pVariantIdType :: DataType Postgres PVariantId
pVariantIdType = DataType pgUuidType

pidType :: DataType Postgres Pid
pidType = DataType pgTextType

svidType :: DataType Postgres Svid
svidType = DataType pgTextType

skuType :: DataType Postgres Sku
skuType = DataType pgTextType

etType :: DataType Postgres EventType
etType = DataType pgTextType

priceType :: DataType Postgres Price
priceType = DataType (numericType pricePrecision)

uidType :: DataType Postgres UserId
uidType = DataType pgUuidType

eidType :: DataType Postgres EventId
eidType = DataType pgSerialType

-- | ---------------------------------------------------------------------------
-- | Migration
-- | ---------------------------------------------------------------------------
migration () =
        SweetSpotDb
                <$> createTable
                            "shops"
                            Shop
                                    { _shopId         = field "shop_id"
                                                              shopIdType
                                                              notNull
                                    , _shopCreated    = field "created"
                                                              timestamptz
                                                              notNull
                                    , _shopShopifyId  = field
                                                                "shopify_id"
                                                                text
                                                                notNull
                                    , _shopName = field "shop_name" text notNull
                                    , _shopClientId   = field "client_id"
                                                              text
                                                              notNull
                                    , _shopOauthToken = field
                                                                "oauth_token"
                                                                text
                                                                notNull
                                    }


                <*> createTable
                            "users"
                            User
                                    { _usrId      = field "user_id" uidType
                                    , _usrCreated = field "created"
                                                          timestamptz
                                                          notNull
                                    }
                <*> createTable
                            "campaigns"
                            Campaign
                                    { _cmpId     = field "campaign_id"
                                                         campaignIdType
                                                         notNull
                                    , _cmpShopId =
                                            ShopKey
                                                    (field "shop_id"
                                                           shopIdType
                                                           notNull
                                                    )
                                    , _cmpName   = field "name" text notNull
                                    , _cmpStart  = field "start" timestamptz
                                    , _cmpEnd    = field "end" timestamptz
                                    }

                <*> createTable
                            "product_variants"
                            ProductVariant
                                    { _pvId        = field "product_variant_id"
                                                           pVariantIdType
                                                           notNull
                                    , _pvShopId    =
                                            ShopKey
                                                    (field "shop_id"
                                                           shopIdType
                                                           notNull
                                                    )
                                    , _pvTitle     = field "title" text notNull
                                    , _pvSku       = field "sku" skuType notNull
                                    , _pvProductId =
                                            field "shopify_product_id"
                                                  pidType
                                                  notNull
                                    , _pvVariantId =
                                            field "shopify_variant_id"
                                                  svidType
                                                  notNull
                                    , _pvPrice = field "price" priceType notNull
                                    , _pvCurrency  = field "currency"
                                                           text
                                                           notNull
                                    }

                <*> createTable
                            "price_variants"
                            PriceVariant
                                    { _prvCmpId            =
                                            CampaignKey
                                                    (field
                                                            "campaign_id"
                                                            campaignIdType
                                                            notNull
                                                    )
                                    , _prvTreatment        = field "treatment"
                                                                   int
                                                                   notNull
                                    , _prvProductVariantId =
                                            PVariantKey
                                                    (field
                                                            "product_variant_id"
                                                            pVariantIdType
                                                            notNull
                                                    )
                                    }
                <*> createTable
                            "user_experiments"
                            UserExperiment
                                    { _ueUserId    =
                                            UserKey
                                                    (field "user_id"
                                                           uidType
                                                           notNull
                                                    )
                                    , _ueCmpId     =
                                            CampaignKey
                                                    (field
                                                            "campaign_id"
                                                            campaignIdType
                                                            notNull
                                                    )
                                    , _ueTreatment = field "treatment"
                                                           int
                                                           notNull
                                    }

                <*> createTable
                            "checkout_events"
                            CheckoutEvent
                                    { _cevId = field "event_id" eidType notNull
                                    , _cevCreated = field "created"
                                                          timestamptz
                                                          notNull
                                    , _cevCmpId   =
                                            CampaignKey
                                                    (field
                                                            "campaign_id"
                                                            campaignIdType
                                                            notNull
                                                    )
                                    , _cevOrderId = field "order_id"
                                                          text
                                                          notNull
                                    , _cevShopId  =
                                            ShopKey
                                                    (field "shop_id"
                                                           shopIdType
                                                           notNull
                                                    )
                                    , _cevUserId  =
                                            UserKey
                                                    (field "user_id"
                                                           uidType
                                                           notNull
                                                    )
                                    , _cevItems   = field
                                                            "items"
                                                            (unboundedArray text)
                                                            notNull
                                    }

                <*> createTable
                            "events"
                            Event { _evId = field "event_id" eidType notNull
                                  , _evPayload = field "payload" jsonb notNull
                                  }

                <*> pgCreateExtension
