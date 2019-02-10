{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Supple.Data.Shopify where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

--
-- Request types
--

--
-- Response types
--
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

data ShopifyProductResponse = ShopifyProductResponse
  { product :: Product
  } deriving (Generic, Show)

instance ToJSON Variant

instance FromJSON Variant

instance ToJSON Product

instance FromJSON Product

instance FromJSON ShopifyResponse

instance FromJSON ShopifyProductResponse
