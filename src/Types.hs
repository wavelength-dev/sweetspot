{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import GHC.Generics (Generic)

data Bucket = Bucket
  { variant_id :: Int
  , sku :: Text
  , price :: Int
  } deriving (Generic, Show)

data UserBucket = UserBucket
  { bucket_price :: Int
  , bucket_variant :: Int
  } deriving (Generic, Show)

data BucketRes = BucketRes
  { message :: Text
  } deriving (Generic, Show)

data UserBucketReq = UserBucketReq
  { user_id :: Int
  , product_sku :: Text
  } deriving (Generic, Show)

instance ToJSON Bucket

instance FromJSON Bucket

instance ToJSON BucketRes

instance FromJSON UserBucketReq

instance ToJSON UserBucket

instance FromRow Bucket where
  fromRow = Bucket <$> field <*> field <*> field

instance FromRow UserBucket where
  fromRow = UserBucket <$> field <*> field
