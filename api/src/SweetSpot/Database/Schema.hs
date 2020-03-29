module SweetSpot.Database.Schema
        ( module SweetSpot.Database.Migrations.V0003AddUserCartTokens
        , migration
        , checkedDb
        , db
        , pgGenUUID_
        , userId_
        , eventId_
        , nonce_
        , shopId_
        , productVariant_
        )
where

import           Control.Arrow                  ( (>>>) )
import           Data.UUID.Types                ( UUID )
import           Database.Beam                  ( DatabaseSettings )
import           Database.Beam.Query.Types      ( QGenExpr )
import           Database.Beam.Query.Internal   ( unsafeRetype )
import           Database.Beam.Migrate          ( CheckedDatabaseSettings
                                                , evaluateDatabase
                                                , unCheckDatabase
                                                , migrationStep
                                                )
import           Database.Beam.Postgres         ( Postgres
                                                , getPgExtension
                                                )
import           Database.Beam.Postgres.PgCrypto
                                                ( PgCrypto(..) )

import qualified SweetSpot.Database.Migrations.V0001InitDb as V1 (migration)
import qualified SweetSpot.Database.Migrations.V0002AddSessions as V2 (migration)
import qualified SweetSpot.Database.Migrations.V0003AddUserCartTokens as V3 (migration)
import           SweetSpot.Data.Common

import SweetSpot.Database.Migrations.V0003AddUserCartTokens hiding (migration)

checkedDb :: CheckedDatabaseSettings Postgres SweetSpotDb
checkedDb = evaluateDatabase migration

db :: DatabaseSettings Postgres SweetSpotDb
db = unCheckDatabase checkedDb

pgGenUUID_ :: QGenExpr ctxt Postgres s UUID
pgGenUUID_ = pgCryptoGenRandomUUID $ getPgExtension (_cryptoExtension db)

userId_ :: QGenExpr ctxt Postgres s UserId
userId_ = unsafeRetype pgGenUUID_

eventId_ :: QGenExpr ctxt Postgres s EventId
eventId_ = unsafeRetype pgGenUUID_

nonce_ :: QGenExpr ctxt Postgres s Nonce
nonce_ = unsafeRetype pgGenUUID_

shopId_ :: QGenExpr ctxt Postgres s ShopId
shopId_ = unsafeRetype pgGenUUID_

productVariant_ :: QGenExpr ctxt Postgres s PVariantId
productVariant_ = unsafeRetype pgGenUUID_

migration =
  migrationStep "Initial schema" V1.migration >>>
  migrationStep "Add sessions" V2.migration >>>
  migrationStep "Add user_cart_tokens" V3.migration
