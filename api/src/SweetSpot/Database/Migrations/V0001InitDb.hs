{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-#  LANGUAGE ImpredicativeTypes #-}

module SweetSpot.Database.Migrations.V0001InitDb where

import           Data.Aeson                     ( Value )
import           Database.Beam
import           Database.Beam.Backend.SQL.Types
                                                ( SqlSerial )
import           Database.Beam.Postgres
import           Database.Beam.Migrate
import           Data.Scientific                ( Scientific )
import           Data.Text                      ( Text )
import           Data.Time                      ( LocalTime )

-- | ---------------------------------------------------------------------------
-- | User
-- | ---------------------------------------------------------------------------
newtype UserT f
  = User
  { _usrId :: Columnar f (SqlSerial Int)
  } deriving (Generic, Beamable)

type User = UserT Identity
type UserKey = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User

instance Table UserT where
        data PrimaryKey UserT f
          = UserKey (Columnar f (SqlSerial Int)) deriving (Generic, Beamable)
        primaryKey = UserKey . _usrId

User (LensFor usrId) = tableLenses

-- | ---------------------------------------------------------------------------
-- | Campaign
-- | ---------------------------------------------------------------------------
data CampaignT f
  = Campaign
  { _cmpId :: Columnar f Text
  , _cmpName :: Columnar f Text
  , _cmpMinProfitIncrease :: Columnar f Int
  , _cmpStartDate :: Columnar f LocalTime
  , _cmpEndDate :: Columnar f LocalTime
  } deriving (Generic, Beamable)

type Campaign = CampaignT Identity
type CampaignKey = PrimaryKey CampaignT Identity

deriving instance Show Campaign

instance Table CampaignT where
        data PrimaryKey CampaignT f
          = CampaignKey (Columnar f Text) deriving (Generic, Beamable)
        primaryKey = CampaignKey . _cmpId

Campaign (LensFor cmpId) (LensFor cmpName) (LensFor cmpMinProfitIncrease) (LensFor cmpStartDate) (LensFor cmpEndDate)
        = tableLenses

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
type ExperimentKey = PrimaryKey ExperimentT Identity

deriving instance Show Experiment

instance Table ExperimentT where
        data PrimaryKey ExperimentT f
          = ExperimentKey (Columnar f (SqlSerial Int)) deriving (Generic, Beamable)
        primaryKey = ExperimentKey . _expId

Experiment (LensFor expId) (LensFor expSku) (LensFor expProductName) =
        tableLenses

-- | ---------------------------------------------------------------------------
-- | Bucket
-- | ---------------------------------------------------------------------------
data BucketT f
  = Bucket
  { _bktId :: Columnar f (SqlSerial Int)
  , _bktType :: Columnar f Text
  , _bktControlSvid :: Columnar f Int
  , _bktTestSvid :: Columnar f Int
  , _bktPrice :: Columnar f Scientific
  } deriving (Generic, Beamable)

type Bucket = BucketT Identity
type BucketKey = PrimaryKey BucketT Identity

deriving instance Show Bucket
deriving instance Eq Bucket

instance Table BucketT where
        data PrimaryKey BucketT f
          = BucketKey (Columnar f (SqlSerial Int)) deriving (Generic, Beamable)
        primaryKey = BucketKey . _bktId

Bucket (LensFor bktId) (LensFor bktType) (LensFor bktControlSvid) (LensFor bktTestSvid) (LensFor bktPrice)
        = tableLenses

-- | ---------------------------------------------------------------------------
-- | BucketUsers
-- | ---------------------------------------------------------------------------
data BucketUserT f
  = BucketUser
  { _bktForUsr :: PrimaryKey BucketT f
  , _usrForBkt :: PrimaryKey UserT f
  } deriving (Generic, Beamable)

type BucketUser = BucketUserT Identity
type BucketUserKey = PrimaryKey BucketUserT Identity

instance Table BucketUserT where
        data PrimaryKey BucketUserT f
          = BucketUserKey (PrimaryKey BucketT f) (PrimaryKey UserT f)
            deriving (Generic, Beamable)
        primaryKey = BucketUserKey <$> _bktForUsr <*> _usrForBkt

BucketUser (BucketKey (LensFor bktForUsr)) (UserKey (LensFor usrForBkt)) =
        tableLenses

-- | ---------------------------------------------------------------------------
-- | CampaignUsers
-- | ---------------------------------------------------------------------------
data CampaignUserT f
  = CampaignUser
  { _cmpForUsr :: PrimaryKey CampaignT f
  , _usrForCmp :: PrimaryKey UserT f
  } deriving (Generic, Beamable)

type CampaignUser = CampaignUserT Identity
type CampaignUserKey = PrimaryKey CampaignUserT Identity

instance Table CampaignUserT where
        data PrimaryKey CampaignUserT f
          = CampaignUserKey (PrimaryKey CampaignT f) (PrimaryKey UserT f)
            deriving (Generic, Beamable)
        primaryKey = CampaignUserKey <$> _cmpForUsr <*> _usrForCmp

CampaignUser (CampaignKey (LensFor cmpForUsr)) (UserKey (LensFor usrForCmp)) =
        tableLenses

-- | ---------------------------------------------------------------------------
-- | CampaignExperiments
-- | ---------------------------------------------------------------------------
data CampaignExperimentT f
  = CampaignExperiment
  { _cmpForExp :: PrimaryKey CampaignT f
  , _expForCmp :: PrimaryKey ExperimentT f
  } deriving (Generic, Beamable)

type CampaignExperiment = CampaignExperimentT Identity
type CampaignExperimentKey = PrimaryKey CampaignExperimentT Identity

instance Table CampaignExperimentT where
        data PrimaryKey CampaignExperimentT f
          = CampaignExperimentKey (PrimaryKey CampaignT f) (PrimaryKey ExperimentT f) deriving (Generic, Beamable)
        primaryKey = CampaignExperimentKey <$> _cmpForExp <*> _expForCmp

CampaignExperiment (CampaignKey (LensFor cmpForExp)) (ExperimentKey (LensFor expForCmp))
        = tableLenses

-- | ---------------------------------------------------------------------------
-- | ExperimentBuckets
-- | ---------------------------------------------------------------------------
data ExperimentBucketT f
  = ExperimentBucket
  { _expForBkt :: PrimaryKey ExperimentT f
  , _bktForExp :: PrimaryKey BucketT f
  } deriving (Generic, Beamable)

type ExperimentBucket = ExperimentBucketT Identity
type ExperimentBucketKey = PrimaryKey ExperimentBucketT Identity

instance Table ExperimentBucketT where
        data PrimaryKey ExperimentBucketT f
          = ExperimentBucketKey (PrimaryKey ExperimentT f) (PrimaryKey BucketT f) deriving (Generic, Beamable)
        primaryKey = ExperimentBucketKey <$> _expForBkt <*> _bktForExp

ExperimentBucket (ExperimentKey (LensFor expForBkt)) (BucketKey (LensFor bktForExp))
        = tableLenses

-- | ---------------------------------------------------------------------------
-- | Events
-- | ---------------------------------------------------------------------------
data EventT f
  = Event
  { _evId :: Columnar f (SqlSerial Int)
  , _evType :: Columnar f Text
  , _evTimestamp :: Columnar f LocalTime
  , _evPayload :: Columnar f (PgJSONB Value)
  } deriving (Generic, Beamable)

type Event = EventT Identity
type EventKey = PrimaryKey EventT Identity

instance Table EventT where
        data PrimaryKey EventT f
          = EventKey (Columnar f (SqlSerial Int)) deriving (Generic, Beamable)
        primaryKey = EventKey . _evId

Event (LensFor evId) (LensFor evType) (LensFor evTimestamp) (LensFor evPayload)
        = tableLenses

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
  , _events :: f (TableEntity EventT)
  } deriving (Generic)

instance Database Postgres SweetSpotDb

SweetSpotDb (TableLens users) (TableLens campaigns) (TableLens experiments) (TableLens buckets) (TableLens bucketUsers) (TableLens campaignUsers) (TableLens campaignExperiments) (TableLens experimentBuckets) (TableLens events)
        = dbLenses

-- | ---------------------------------------------------------------------------
-- | Price
-- | ---------------------------------------------------------------------------
pricePrecision :: Maybe (Word, Maybe Word)
pricePrecision = Just (12, Just 2)

-- | ---------------------------------------------------------------------------
-- | Migration
-- | ---------------------------------------------------------------------------
migration () =
        SweetSpotDb
                <$> createTable "users" User { _usrId = field "user_id" serial }
                <*> createTable
                            "campaigns"
                            Campaign
                                    { _cmpId = field "campaign_id" text notNull
                                    , _cmpName = field "name" text notNull
                                    , _cmpMinProfitIncrease =
                                            field "min_profit_increase"
                                                  int
                                                  notNull
                                    , _cmpStartDate = field
                                                              "start_date"
                                                              timestamptz
                                                              notNull
                                    , _cmpEndDate = field "end_date"
                                                          timestamptz
                                                          notNull
                                    }

                <*> createTable
                            "experiments"
                            Experiment
                                    { _expId = field "exp_id" serial notNull
                                    , _expSku = field "sku" text notNull
                                    , _expProductName = field
                                                                "product_name"
                                                                text
                                                                notNull
                                    }

                <*> createTable
                            "buckets"
                            Bucket
                                    { _bktId = field "bucket_id" serial notNull
                                    , _bktType        = field "bucket_type"
                                                              text
                                                              notNull
                                    , _bktControlSvid = field
                                                                "original_svid"
                                                                bigint
                                                                notNull
                                    , _bktTestSvid    = field "test_svid"
                                                              bigint
                                                              notNull
                                    , _bktPrice       =
                                            field
                                                    "price"
                                                    (numeric pricePrecision)
                                                    notNull
                                    }
                <*> createTable
                            "bucket_users"
                            BucketUser
                                    { _bktForUsr =
                                            BucketKey
                                                    (field "bucket_id"
                                                           serial
                                                           notNull
                                                    )
                                    , _usrForBkt =
                                            UserKey
                                                    (field "user_id"
                                                           serial
                                                           notNull
                                                    )
                                    }

                <*> createTable
                            "campaign_users"
                            CampaignUser
                                    { _cmpForUsr =
                                            CampaignKey
                                                    (field "campaign_id"
                                                           text
                                                           notNull
                                                    )
                                    , _usrForCmp =
                                            UserKey
                                                    (field "user_id"
                                                           serial
                                                           notNull
                                                    )
                                    }

                <*> createTable
                            "campaign_experiments"
                            CampaignExperiment
                                    { _cmpForExp =
                                            CampaignKey
                                                    (field "campaign_id"
                                                           text
                                                           notNull
                                                    )
                                    , _expForCmp =
                                            ExperimentKey
                                                    (field "exp_id"
                                                           serial
                                                           notNull
                                                    )
                                    }

                <*> createTable
                            "experiment_buckets"
                            ExperimentBucket
                                    { _expForBkt =
                                            ExperimentKey
                                                    (field "exp_id"
                                                           serial
                                                           notNull
                                                    )
                                    , _bktForExp =
                                            BucketKey
                                                    (field "bucket_id"
                                                           serial
                                                           notNull
                                                    )
                                    }
                <*> createTable
                            "events"
                            Event
                                    { _evId        = field "id" serial notNull
                                    , _evType      = field "type" text notNull
                                    , _evTimestamp = field "timestamp"
                                                           timestamptz
                                                           notNull
                                    , _evPayload = field "payload" jsonb notNull
                                    }
