{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SweetSpot.Database.Schema
        ( module SweetSpot.Database.Migrations.V0001InitDb
        , migration
        , checkedDb
        , db
        , pgGenUUID_
        , userId_
        , eventId_
        )
where

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

import qualified SweetSpot.Database.Migrations.V0001InitDb
                                               as V1
import           SweetSpot.Database.Migrations.V0001InitDb
                                         hiding ( migration )
import           SweetSpot.Data.Common          ( UserId(..)
                                                , EventId(..)
                                                )

checkedDb :: CheckedDatabaseSettings Postgres V1.SweetSpotDb
checkedDb = evaluateDatabase migration

db :: DatabaseSettings Postgres V1.SweetSpotDb
db = unCheckDatabase checkedDb

pgGenUUID_ :: QGenExpr ctxt Postgres s UUID
pgGenUUID_ = pgCryptoGenRandomUUID $ getPgExtension (_cryptoExtension db)

userId_ :: QGenExpr ctxt Postgres s UserId
userId_ = unsafeRetype pgGenUUID_

eventId_ :: QGenExpr ctxt Postgres s EventId
eventId_ = unsafeRetype pgGenUUID_

migration = migrationStep "Initial schema" V1.migration
