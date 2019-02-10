{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Supple.Data.Api where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Control.Lens
import Data.Aeson.Lens
import Data.Text (Text)
import GHC.Generics (Generic)
import Supple.Data.Common (Price)

--
-- Request types
--
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
  | Unknown UnknownView
  deriving (Show)

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

instance FromJSON TrackView where
  parseJSON val = case page of
    "product" -> Details <$> parseJSON val
    "collection" -> Listing <$> parseJSON val
    "collections" -> Collection <$> parseJSON val
    "unknown" -> Unknown <$> parseJSON val
    where
      page = val ^. key "page" . _String

instance ToJSON TrackView where
  toJSON view = case view of
    Details a -> toJSON a
    Listing a -> toJSON a
    Collection a -> toJSON a
    Unknown a -> toJSON a
--
-- Response types
--
data OkResponse = OkResponse
  { message :: Text
  } deriving (Generic, Show)

instance ToJSON OkResponse
