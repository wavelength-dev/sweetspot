module SweetSpot.Database.Schema
  ( module SweetSpot.Database.Migrations.V0003AddUserCartTokens,
    migration,
    checkedDb,
    db,
    pgGenUUID_,
    eventId_,
    nonce_,
    shopId_,
    campaignId_,
    productVariant_,
    nowUTC_,
  )
where

import Control.Arrow ((>>>))
import Data.Time (UTCTime)
import Data.UUID.Types (UUID)
import Database.Beam (DatabaseSettings, QExpr)
import Database.Beam.Backend.SQL.SQL92
  ( timestampType,
  )
import Database.Beam.Migrate
  ( CheckedDatabaseSettings,
    HasDefaultSqlDataType (..),
    evaluateDatabase,
    migrationStep,
    unCheckDatabase,
  )
import Database.Beam.Postgres
  ( Postgres,
    getPgExtension,
    now_,
  )
import Database.Beam.Postgres.PgCrypto
  ( PgCrypto (..),
  )
import Database.Beam.Query.Internal (unsafeRetype)
import Database.Beam.Query.Types (QGenExpr)
import RIO
import SweetSpot.Data.Common
import qualified SweetSpot.Database.Migrations.V0001InitDb as V1 (migration)
import qualified SweetSpot.Database.Migrations.V0002AddSessions as V2 (migration)
import qualified SweetSpot.Database.Migrations.V0003AddUserCartTokens as V3 (migration)
import SweetSpot.Database.Migrations.V0003AddUserCartTokens hiding (migration)

checkedDb :: CheckedDatabaseSettings Postgres SweetSpotDb
checkedDb = evaluateDatabase migration

db :: DatabaseSettings Postgres SweetSpotDb
db = unCheckDatabase checkedDb

pgGenUUID_ :: QGenExpr ctxt Postgres s UUID
pgGenUUID_ = pgCryptoGenRandomUUID $ getPgExtension (_cryptoExtension db)

eventId_ :: QGenExpr ctxt Postgres s EventId
eventId_ = unsafeRetype pgGenUUID_

nonce_ :: QGenExpr ctxt Postgres s Nonce
nonce_ = unsafeRetype pgGenUUID_

shopId_ :: QGenExpr ctxt Postgres s ShopId
shopId_ = unsafeRetype pgGenUUID_

campaignId_ :: QGenExpr ctxt Postgres s CampaignId
campaignId_ = unsafeRetype pgGenUUID_

productVariant_ :: QGenExpr ctxt Postgres s PVariantId
productVariant_ = unsafeRetype pgGenUUID_

nowUTC_ :: QExpr Postgres s UTCTime
nowUTC_ = unsafeRetype now_

instance HasDefaultSqlDataType Postgres UTCTime where
  defaultSqlDataType _ _ _ = timestampType Nothing True

migration =
  migrationStep "Initial schema" V1.migration
    >>> migrationStep "Add sessions" V2.migration
    >>> migrationStep "Add user_cart_tokens" V3.migration
