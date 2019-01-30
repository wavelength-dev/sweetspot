{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Supple.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Generics (Generic)

type Price = Scientific

data UserBucket = UserBucket
  { user_id :: !Int
  , bucket_sku :: !Text
  , bucket_svid :: !Int
  , bucket_price :: !Price
  } deriving (Generic, Show)

instance ToJSON UserBucket

data Variant = Variant
  { id :: !Int
  , product_id :: !Int
  , title :: !Text
  , sku :: !Text
  } deriving (Generic, Show)

instance ToJSON Variant

instance FromJSON Variant

data Product = Product
  { id :: !Int
  , title :: !Text
  , variants :: ![Variant]
  } deriving (Generic, Show)

instance ToJSON Product

instance FromJSON Product

data ShopifyResponse = ShopifyResponse
  { products :: ![Product]
  } deriving (Generic, Show)

instance FromJSON ShopifyResponse

data CreateVariant = CreateVariant
  { option1 :: Text
  , price :: !Price
  } deriving (Generic, Show)

instance ToJSON CreateVariant

instance FromJSON CreateVariant

data ShopifyVariantBody = ShopifyVariantBody
  { variant :: CreateVariant
  } deriving (Generic, Show)

instance ToJSON ShopifyVariantBody

data OkResponse = OkResponse
  { message :: Text
  } deriving (Generic, Show)

instance ToJSON OkResponse

data Bucket = Bucket
  { bucket_id :: !Int
  , svid :: !Int
  , price :: !Price
  } deriving (Generic, Show)

instance ToJSON Bucket

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

instance ToJSON ExperimentBuckets
