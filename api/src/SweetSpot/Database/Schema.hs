module SweetSpot.Database.Schema
  ( module SweetSpot.Database.Migration.Init,
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
import SweetSpot.Database.Migration.Init

checkedDb :: CheckedDatabaseSettings Postgres SweetSpotDb
checkedDb = evaluateDatabase schema
  where
    schema = migrationStep "Initial schema" migration

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
