{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SweetSpot.Database.Schema
        ( module SweetSpot.Database.Migrations.V0001InitDb
        , migration
        , checkedDb
        , db
        )
where

import           Database.Beam                  ( DatabaseSettings )
import           Database.Beam.Migrate          ( CheckedDatabaseSettings
                                                , evaluateDatabase
                                                , unCheckDatabase
                                                , migrationStep
                                                )
import           Database.Beam.Migrate.Simple   ( createSchema, bringUpToDate )
import           Database.Beam.Postgres         ( Postgres
                                                , runBeamPostgres
                                                , Connection
                                                , connect
                                                , defaultConnectInfo
                                                )
import           Database.Beam.Postgres.Migrate ( migrationBackend )

import qualified SweetSpot.Database.Migrations.V0001InitDb
                                               as V1
import           SweetSpot.Database.Migrations.V0001InitDb
                                         hiding ( migration )

checkedDb :: CheckedDatabaseSettings Postgres V1.SweetSpotDb
checkedDb = evaluateDatabase migration

db :: DatabaseSettings Postgres V1.SweetSpotDb
db = unCheckDatabase checkedDb


migration = migrationStep "Initial schema" V1.migration

createDbSchema :: Connection -> IO ()
createDbSchema conn = runBeamPostgres
        conn
        (createSchema migrationBackend (evaluateDatabase migration))
