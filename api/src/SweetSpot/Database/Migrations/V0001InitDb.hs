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
import           Data.Time                      ( UTCTime )

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
-- | Campaign
-- | ---------------------------------------------------------------------------
data CampaignT f
  = Campaign
  { _cmpId :: Columnar f Text
  , _cmpName :: Columnar f Text
  , _cmpMinProfitIncrease :: Columnar f Int
  , _cmpStartDate :: Columnar f UTCTime
  , _cmpEndDate :: Columnar f UTCTime
  } deriving (Generic, Beamable)

type Campaign = CampaignT Identity
type CampaignId = PrimaryKey CampaignT Identity

instance Table CampaignT where
        data PrimaryKey CampaignT f
          = CampaignId (Columnar f Text) deriving (Generic, Beamable)
        primaryKey = CampaignId . _cmpId

-- | ---------------------------------------------------------------------------
-- | Experiment
-- | ---------------------------------------------------------------------------
data ExperimentT f
  = Experiment
  { _expId :: Columnar f (SqlSerial Int)
  , _expSku :: Columnar f Text
  , _expProductName :: Columnar f Text
  } deriving (Generic, Beamable)

type Experiment = ExperimentT Identity
type ExperimentId = PrimaryKey ExperimentT Identity

instance Table ExperimentT where
        data PrimaryKey ExperimentT f
          = ExperimentId (Columnar f (SqlSerial Int)) deriving (Generic, Beamable)
        primaryKey = ExperimentId . _expId

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
-- | BucketUsers
-- | ---------------------------------------------------------------------------
data BucketUserT f
  = BucketUser
  { _bucketForUser :: PrimaryKey BucketT f
  , _userForBucket :: PrimaryKey UserT f
  } deriving (Generic, Beamable)

type BucketUser = BucketUserT Identity

-- deriving instance Show BucketUser
-- deriving instance Eq BucketUser

-- | ---------------------------------------------------------------------------
-- | CampaignUsers
-- | ---------------------------------------------------------------------------
data CampaignUserT f
  = CampaignUser
  { _campaignForUser :: PrimaryKey CampaignT f
  , _userForCampaign :: PrimaryKey UserT f
  } deriving (Generic, Beamable)

type CampaignUser = CampaignUserT Identity

-- deriving instance Show CampaignUser
-- deriving instance Eq CampaignUser

-- | ---------------------------------------------------------------------------
-- | CampaignExperiments
-- | ---------------------------------------------------------------------------
data CampaignExperimentT f
  = CampaignExperiment
  { _campaignForExperiment :: PrimaryKey CampaignT f
  , _experimentForCampaign :: PrimaryKey ExperimentT f
  } deriving (Generic, Beamable)

type CampaignExperiment = CampaignExperimentT Identity

-- deriving instance Show CampaignExperiment
-- deriving instance Eq CampaignExperiment

-- | ---------------------------------------------------------------------------
-- | ExperimentBuckets
-- | ---------------------------------------------------------------------------
data ExperimentBucketT f
  = ExperimentBucket
  { _experimentForBucket :: PrimaryKey ExperimentT f
  , _bucketForExperiment :: PrimaryKey BucketT f
  } deriving (Generic, Beamable)

type ExperimentBucket = ExperimentBucketT Identity

-- deriving instance Show ExperimentBucket
-- deriving instance Eq ExperimentBucket

-- | ---------------------------------------------------------------------------
-- | Database
-- | ---------------------------------------------------------------------------
data SweetSpotDb f = SweetSpotDb
  { _users :: f (TableEntity UserT)
  , _campaigns :: f (TableEntity CampaignT)
  , _experiments :: f (TableEntity ExperimentT)
  , _buckets :: f (TableEntity BucketT)
  , _bucketUsers :: f (TableEntity BucketUserT)
  , _campaignUsers :: f (TableEntity CampaignUserT)
  , _campaignExperiments :: f (TableEntity CampaignExperimentT)
  , _experimentBuckets :: f (TableEntity ExperimentBucketT)
  } deriving (Generic)

instance Database Postgres SweetSpotDb


-- | ---------------------------------------------------------------------------
-- | Migration
-- | ---------------------------------------------------------------------------
migration () =
        SweetSpotDb
                <$> createTable "users" (User (field "user_id" serial))
                <*> "campaigns"
                            (Campaign (field "campaign_id" text)
                                      (field "name" text)
                                      (field "min_profit_increase" int)
                                      (field "start_date")
                            )
                <*> "buckets"
                            (Bucket (field "id" serial)
                                    (field "type" text notNull)
                                    (field "original_svid" int notNull)
                                    (field "test_svid" int notNull)
                                    (field "price" double notNull)
                            )
