{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SweetSpot.Database.Beam where

import           Database.Beam
import           Database.Beam.Backend.SQL.Types
                                                ( SqlSerial )
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Migrate ( getDbConstraints
                                                , migrationBackend
                                                )
import           Database.Beam.Migrate
import           Database.Beam.Migrate.Simple   ( createSchema
                                                , verifySchema
                                                )

import           Data.Text                      ( Text )


-- | ---------------------------------------------------------------------------
-- | Bucket
-- | ---------------------------------------------------------------------------
data BucketT f
  = Bucket
  { _bucketId :: Columnar f (SqlSerial Int)
  , _bucketType :: Columnar f Text
  , _bucketOriginalSvid :: Columnar f Int
  , _bucketTestSvid :: Columnar f Int
  , _bucketPrice :: Columnar f Double
  } deriving (Generic, Beamable)

type Bucket = BucketT Identity
type BucketId = PrimaryKey BucketT Identity

deriving instance Show Bucket
deriving instance Eq Bucket

instance Table BucketT where
        data PrimaryKey BucketT f
          = BucketId (Columnar f (SqlSerial Int)) deriving (Generic, Beamable)
        primaryKey = BucketId . _bucketId


-- | ---------------------------------------------------------------------------
-- | User
-- | ---------------------------------------------------------------------------
newtype UserT f
  = User
  { _userId :: Columnar f (SqlSerial Int)
  } deriving (Generic, Beamable)

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User

instance Table UserT where
        data PrimaryKey UserT f
          = UserId (Columnar f (SqlSerial Int)) deriving (Generic, Beamable)
        primaryKey = UserId . _userId

-- | ---------------------------------------------------------------------------
-- | Database
-- | ---------------------------------------------------------------------------
data SweetSpotDb f = SweetSpotDb
  { _buckets :: f (TableEntity BucketT)
  , _users :: f (TableEntity UserT)
  } deriving (Generic)

instance Database Postgres SweetSpotDb

initMigration () =
        SweetSpotDb
                <$> createTable
                            "buckets"
                            (Bucket (field "id" serial)
                                    (field "type" text notNull)
                                    (field "original_svid" int notNull)
                                    (field "test_svid" int notNull)
                                    (field "price" double notNull)
                            )

                <*> createTable "users" (User (field "id" serial))


migration = migrationStep "Initial schema" initMigration


sweetspotChecked :: CheckedDatabaseSettings Postgres SweetSpotDb
sweetspotChecked = evaluateDatabase migration

sweetspot :: DatabaseSettings Postgres SweetSpotDb
sweetspot = unCheckDatabase sweetspotChecked

-- | ---------------------------------------------------------------------------
-- | Queries
-- | ---------------------------------------------------------------------------
create :: Connection -> IO ()
create conn =
        runBeamPostgres conn (createSchema migrationBackend sweetspotChecked)

addBuckets :: Connection -> IO ()
addBuckets conn =
        runBeamPostgresDebug putStrLn conn
                $ runInsert
                $ insert (_buckets sweetspot)
                $ insertValues
                          [ Bucket 123 "control" 12345 12346 10.90
                          , Bucket 124 "test"    22345 22346 15.90
                          ]

addUsers :: Connection -> IO ()
addUsers conn =
        runBeamPostgresDebug putStrLn conn
                $ runInsert
                $ insert (_users sweetspot)
                $ insertExpressions [User default_, User default_]

getAllBuckets :: Connection -> IO ()
getAllBuckets conn = runBeamPostgresDebug putStrLn conn $ do
        buckets <- runSelectReturningList $ select allBuckets
        mapM_ (liftIO . print) buckets
        where allBuckets = all_ (_buckets sweetspot)


getAllUsers :: Connection -> IO ()
getAllUsers conn = runBeamPostgresDebug putStrLn conn $ do
        users <- runSelectReturningList $ select allUsers
        mapM_ (liftIO . print) users
        where allUsers = all_ (_users sweetspot)


verify :: Connection -> IO ()
verify conn = do
        res <- runBeamPostgres
                conn
                (verifySchema migrationBackend sweetspotChecked)
        print res
        return ()
