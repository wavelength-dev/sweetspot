{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Supple.Data.Shopify where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Supple.Data.Api (Product)

data ShopifyResponse = ShopifyResponse
  { products :: ![Product]
  } deriving (Generic, Show)

data ShopifyProductResponse = ShopifyProductResponse
  { product :: Product
  } deriving (Generic, Show)

instance FromJSON ShopifyResponse

instance FromJSON ShopifyProductResponse
