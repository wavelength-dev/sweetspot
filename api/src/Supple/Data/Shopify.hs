{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Supple.Data.Shopify where

import Data.Aeson (FromJSON, ToJSON, genericParseJSON, parseJSON)
import Data.Aeson.Types (defaultOptions, fieldLabelModifier)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Casing (quietSnake)

--
-- Request types
--
--
-- Response types
--
data Image = Image
  { src :: !Text
  } deriving (Generic, Show)

data Variant = Variant
  { id :: !Int
  , productId :: !Int
  , title :: !Text
  , sku :: !Text
  } deriving (Generic, Show)

data Product = Product
  { id :: !Int
  , title :: !Text
  , variants :: ![Variant]
  , image :: !Image
  } deriving (Generic, Show)

data ShopifyResponse = ShopifyResponse
  { products :: ![Product]
  } deriving (Generic, Show)

data ShopifyProductResponse = ShopifyProductResponse
  { product :: Product
  } deriving (Generic, Show)

instance ToJSON Variant

instance FromJSON Variant where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = quietSnake}

instance ToJSON Image

instance FromJSON Image

instance ToJSON Product

instance FromJSON Product

instance FromJSON ShopifyResponse

instance FromJSON ShopifyProductResponse
