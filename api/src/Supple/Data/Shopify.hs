{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Supple.Data.Shopify where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Supple.Data.Api (CreateVariant)

--
-- Request types
--
data ShopifyVariantBody = ShopifyVariantBody
  { variant :: CreateVariant
  } deriving (Generic, Show)

instance ToJSON ShopifyVariantBody

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

instance ToJSON Variant

instance FromJSON Variant

instance ToJSON Product

instance FromJSON Product

instance FromJSON ShopifyResponse
