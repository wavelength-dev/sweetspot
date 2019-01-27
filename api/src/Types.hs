{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Scientific (Scientific)
import GHC.Generics (Generic)

type Price = Scientific

data UserBucket = UserBucket
  { user_id :: !Int
  , bucket_sku :: !Text
  , bucket_svid :: !Int
  , bucket_price :: !Price
  } deriving (Generic, Show)

data Variant = Variant
  { id :: !Int
  , product_id :: !Int
  , title :: !Text
  , sku :: !Text
  } deriving (Generic, Show)

data Product = Product
  { id :: !Int
  , title :: !Text
  , variants :: ![Variant]
  } deriving (Generic, Show)

data ShopifyResponse = ShopifyResponse
  { products :: ![Product]
  } deriving (Generic, Show)

data CreateVariant = CreateVariant
  { option1 :: Text
  , price :: !Price
  } deriving (Generic, Show)

data ShopifyVariantBody = ShopifyVariantBody
  { variant :: CreateVariant
  } deriving (Generic, Show)

data OkResponse = OkResponse
  { message :: Text
  } deriving (Generic, Show)

instance ToJSON UserBucket

instance ToJSON CreateVariant

instance ToJSON ShopifyVariantBody

instance ToJSON OkResponse

instance ToJSON Variant

instance ToJSON Product

instance FromJSON Variant

instance FromJSON CreateVariant

instance FromJSON Product

instance FromJSON ShopifyResponse
