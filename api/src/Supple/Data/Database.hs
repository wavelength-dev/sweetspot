{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Supple.Data.Database where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Supple.Data.Common (Price, EventType(..))

data Bucket = Bucket
  { bucket_id :: !Int
  , svid :: !Int
  , price :: !Price
  } deriving (Generic, Show)

data Experiment = Experiment
  { exp_id :: !Int
  , sku :: !Text
  , name :: !Text
  } deriving (Generic, Show)

data ExperimentBuckets = ExperimentBuckets
  { exp_id :: !Int
  , sku :: !Text
  , name :: !Text
  , buckets :: ![Bucket]
  } deriving (Generic, Show)

data UserBucket = UserBucket
  { user_id :: !Int
  , bucket_sku :: !Text
  , bucket_svid :: !Int
  , bucket_price :: !Price
  } deriving (Generic, Show)

instance ToJSON UserBucket

eventTypeToText :: EventType -> Text
eventTypeToText View = "view"

instance ToJSON ExperimentBuckets

instance ToJSON Bucket
