{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Supple.Data.Api where

import Control.Lens
import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON)
import Data.Aeson.Lens
import Data.Aeson.Types (defaultOptions, fieldLabelModifier, typeMismatch)
import Data.Text (Text)
import GHC.Generics (Generic)
import Supple.Data.Common (Price)
import Text.Casing (quietSnake)

--
-- Request types
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

data CreateExperiment = CreateExperiment
  { productId :: !Int
  , price :: !Price
  , name :: !Text
  } deriving (Generic, Show)

data ProductDetailsView = ProductDetailsView
  { campaign :: Maybe Text
  , page :: !Text -- product
  , pageUrl :: !Text
  , productId :: Int
  , userId :: Maybe Text
  } deriving (Generic, Show)

data ProductListingsView = ProductListingsView
  { campaign :: Maybe Text
  , page :: Text -- collection
  , pageUrl :: Text
  , productIds :: [Int]
  , userId :: Maybe Text
  } deriving (Generic, Show)

data CollectionListingsView = CollectionListingsView
  { campaign :: Maybe Text
  , page :: Text -- collections
  , pageUrl :: Text
  , userId :: Maybe Text
  } deriving (Generic, Show)

data LineItem = LineItem
  { product_id :: Int
  , variant_id :: Int
  } deriving (Generic, Show)

data CheckoutEvent = CheckoutEvent
  { lineItems :: [LineItem]
  , page :: Text -- checkout
  , pageUrl :: Text
  , step :: Maybe Text
  , token :: Maybe Text
  , userId :: Maybe Text
  } deriving (Generic, Show)

data UnknownView = UnknownView
  { campaign :: Maybe Text
  , page :: Text -- unknown
  , pageUrl :: Text
  , userId :: Maybe Text
  } deriving (Generic, Show)

data TrackView
  = Details ProductDetailsView
  | Listing ProductListingsView
  | Collection CollectionListingsView
  | Checkout CheckoutEvent
  | Unknown UnknownView
  deriving (Show)

instance ToJSON Variant

instance FromJSON Variant where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = quietSnake}

instance ToJSON Image

instance FromJSON Image

instance ToJSON Product

instance FromJSON Product

instance FromJSON ProductDetailsView

instance ToJSON ProductDetailsView

instance FromJSON ProductListingsView

instance ToJSON ProductListingsView

instance FromJSON CollectionListingsView

instance ToJSON CollectionListingsView

instance FromJSON UnknownView

instance ToJSON UnknownView

instance ToJSON CreateExperiment

instance FromJSON CreateExperiment

instance FromJSON LineItem where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = quietSnake}

instance ToJSON LineItem

instance FromJSON CheckoutEvent

instance ToJSON CheckoutEvent

instance FromJSON TrackView where
  parseJSON val =
    case page of
      "product" -> Details <$> parseJSON val
      "collection" -> Listing <$> parseJSON val
      "collections" -> Collection <$> parseJSON val
      "checkout" -> Checkout <$> parseJSON val
      "unknown" -> Unknown <$> parseJSON val
      _ -> typeMismatch "TrackView" val
    where
      page = val ^. key "page" . _String

instance ToJSON TrackView where
  toJSON view =
    case view of
      Details a -> toJSON a
      Listing a -> toJSON a
      Collection a -> toJSON a
      Checkout a -> toJSON a
      Unknown a -> toJSON a

--
-- Response types
--
data OkResponse = OkResponse
  { message :: Text
  } deriving (Generic, Show)

instance ToJSON OkResponse
