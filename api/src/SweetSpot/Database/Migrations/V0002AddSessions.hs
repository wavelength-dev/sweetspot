{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}

module SweetSpot.Database.Migrations.V0002AddSessions
  ( module SweetSpot.Database.Migrations.V0001InitDb,
    module SweetSpot.Database.Migrations.V0002AddSessions,
  )
where

import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Postgres (PgExtensionEntity, Postgres)
import Database.Beam.Postgres.PgCrypto (PgCrypto)
import Database.Beam.Postgres.Syntax (pgTextType)
import SweetSpot.Data.Common
import SweetSpot.Database.Migrations.V0001InitDb hiding
  ( SweetSpotDb (..),
    campaigns,
    checkoutEvents,
    checkoutItems,
    cryptoExtension,
    events,
    installNonces,
    migration,
    productVariants,
    shops,
    treatments,
    userExperiments,
    users,
  )
import qualified SweetSpot.Database.Migrations.V0001InitDb as V1

-- | ---------------------------------------------------------------------------
-- | Session
-- | ---------------------------------------------------------------------------
data SessionT f
  = Session
      { _sessionId :: Columnar f SessionId,
        _sessionShopId :: PrimaryKey V1.ShopT f
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

Session (LensFor sessionId) (V1.ShopKey (LensFor sessionShopDomain)) = tableLenses

sessionIdType :: DataType Postgres SessionId
sessionIdType = DataType pgTextType

-- | ---------------------------------------------------------------------------
-- | Database
-- | ---------------------------------------------------------------------------
data SweetSpotDb f
  = SweetSpotDb
      { _shops :: f (TableEntity V1.ShopT),
        _installNonces :: f (TableEntity V1.InstallNonceT),
        _users :: f (TableEntity V1.UserT),
        _campaigns :: f (TableEntity V1.CampaignT),
        _productVariants :: f (TableEntity V1.ProductVariantT),
        _treatments :: f (TableEntity V1.TreatmentT),
        _userExperiments :: f (TableEntity V1.UserExperimentT),
        _checkoutEvents :: f (TableEntity V1.CheckoutEventT),
        _checkoutItems :: f (TableEntity V1.CheckoutItemT),
        _events :: f (TableEntity V1.EventT),
        _cryptoExtension :: f (PgExtensionEntity PgCrypto),
        _sessions :: f (TableEntity SessionT)
      }
  deriving (Generic)

instance Database Postgres SweetSpotDb

SweetSpotDb (TableLens shops) (TableLens installNonces) (TableLens users) (TableLens campaigns) (TableLens productVariants) (TableLens treatments) (TableLens userExperiments) (TableLens checkoutEvents) (TableLens checkoutItems) (TableLens events) (TableLens cryptoExtension) (TableLens sessions) = dbLenses

-- | ---------------------------------------------------------------------------
-- | Migration
-- | ---------------------------------------------------------------------------
migration ::
  CheckedDatabaseSettings Postgres V1.SweetSpotDb ->
  Migration Postgres (CheckedDatabaseSettings Postgres SweetSpotDb)
migration currentDb =
  SweetSpotDb
    <$> preserve (V1._shops currentDb)
    <*> preserve (V1._installNonces currentDb)
    <*> preserve (V1._users currentDb)
    <*> preserve (V1._campaigns currentDb)
    <*> preserve (V1._productVariants currentDb)
    <*> preserve (V1._treatments currentDb)
    <*> preserve (V1._userExperiments currentDb)
    <*> preserve (V1._checkoutEvents currentDb)
    <*> preserve (V1._checkoutItems currentDb)
    <*> preserve (V1._events currentDb)
    <*> preserve (V1._cryptoExtension currentDb)
    <*> createTable
      "sessions"
      Session
        { _sessionId = field "session_id" sessionIdType notNull unique,
          _sessionShopId = V1.ShopKey $ field "shop_id" V1.shopIdType notNull
        }
