{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Supple.Data.Database where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Supple.Data.Common

data Bucket = Bucket
  { bucketId :: !BucketId
  , svid :: !Svid
  , price :: !Price
  } deriving (Generic, Show)

data Experiment = Experiment
  { expId :: !ExpId
  , sku :: !Sku
  , name :: !Text
  } deriving (Generic, Show)

data ExperimentBuckets = ExperimentBuckets
  { expId :: !ExpId
  , sku :: !Sku
  , name :: !Text
  , buckets :: ![Bucket]
  } deriving (Generic, Show)

data UserBucket = UserBucket
  { userId :: !UserId
  , bucketSku :: !Sku
  , bucketSvid :: !Svid
  , bucketPrice :: !Price
  } deriving (Generic, Show)

instance ToJSON UserBucket

eventTypeToText :: EventType -> Text
eventTypeToText View = "view"

instance ToJSON ExperimentBuckets

instance ToJSON Bucket
