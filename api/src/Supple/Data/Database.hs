{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Supple.Data.Database where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Supple.Data.Common

data Bucket = Bucket
  { bucket_id :: !BucketId
  , svid :: !Svid
  , price :: !Price
  } deriving (Generic, Show)

data Experiment = Experiment
  { exp_id :: !ExpId
  , sku :: !Sku
  , name :: !Text
  } deriving (Generic, Show)

data ExperimentBuckets = ExperimentBuckets
  { exp_id :: !ExpId
  , sku :: !Sku
  , name :: !Text
  , buckets :: ![Bucket]
  } deriving (Generic, Show)

data UserBucket = UserBucket
  { user_id :: !UserId
  , bucket_sku :: !Sku
  , bucket_svid :: !Svid
  , bucket_price :: !Price
  } deriving (Generic, Show)

instance ToJSON UserBucket

eventTypeToText :: EventType -> Text
eventTypeToText View = "view"

instance ToJSON ExperimentBuckets

instance ToJSON Bucket
