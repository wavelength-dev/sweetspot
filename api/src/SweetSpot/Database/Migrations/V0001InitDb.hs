{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SweetSpot.Database.Migrations.V0001InitDb where

import           Database.Beam
import           Database.Beam.Backend.SQL.Types
                                                ( SqlSerial )
import           Database.Beam.Postgres
import           Database.Beam.Migrate
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


-- | ---------------------------------------------------------------------------
-- | Migration
-- | ---------------------------------------------------------------------------
migration () =
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
