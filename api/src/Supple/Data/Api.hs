{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Supple.Data.Api where

import Control.Lens
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON)
import Data.Aeson.Lens
import Data.Aeson.Types (defaultOptions, fieldLabelModifier, typeMismatch)
import Data.Text (Text)
import GHC.Generics (Generic)
import Supple.Data.Common
import Text.Casing (quietSnake)

-- | ---------------------------------------------------------------------------
-- | Image
-- | ---------------------------------------------------------------------------
data Image = Image
  { _iSrc :: !Text
  } deriving (Eq, Generic, Show)

makeLenses ''Image

instance FromJSON Image

instance ToJSON Image

-- | ---------------------------------------------------------------------------
-- | Variant
-- | ---------------------------------------------------------------------------
data Variant = Variant
  { _vId :: !Svid
  , _vProductId :: !Pid
  , _vTitle :: !Text
  , _vSku :: !Sku
  } deriving (Eq, Generic, Show)

makeLenses ''Variant

instance ToJSON Variant

instance FromJSON Variant

-- | ---------------------------------------------------------------------------
-- | Product
-- | ---------------------------------------------------------------------------
data Product = Product
  { _pId :: !Pid
  , _pTitle :: !Text
  , _pVariants :: ![Variant]
  , _pImage :: !Image
  } deriving (Eq, Generic, Show)

makeLenses ''Product

instance ToJSON Product

instance FromJSON Product

-- | ---------------------------------------------------------------------------
-- | Bucket
-- | ---------------------------------------------------------------------------
data Bucket = Bucket
  { _bBucketId :: !BucketId
  , _bSvid :: !Svid
  , _bPrice :: !Price
  } deriving (Eq, Generic, Show)

makeLenses ''Bucket

instance ToJSON Bucket

instance FromJSON Bucket

-- | ---------------------------------------------------------------------------
-- | Experiment
-- | ---------------------------------------------------------------------------
data Experiment = Experiment
  { _eExpId :: !ExpId
  , _eSku :: !Sku
  , _eName :: !Text
  } deriving (Eq, Generic, Show)

makeLenses ''Experiment

-- | ---------------------------------------------------------------------------
-- | ExperimentBuckets
-- | ---------------------------------------------------------------------------
data ExperimentBuckets = ExperimentBuckets
  { _ebExpId :: !ExpId
  , _ebSku :: !Sku
  , _ebName :: !Text
  , _ebBuckets :: ![Bucket]
  } deriving (Eq, Generic, Show)

makeLenses ''ExperimentBuckets

instance ToJSON ExperimentBuckets

-- | ---------------------------------------------------------------------------
-- | UserBucket
-- | ---------------------------------------------------------------------------
data UserBucket = UserBucket
  { _ubUserId :: !UserId
  , _ubSku :: !Sku
  , _ubSvid :: !Svid
  , _ubPrice :: !Price
  , _ubExpId :: !ExpId
  } deriving (Eq, Generic, Show)

makeLenses ''UserBucket

instance ToJSON UserBucket

-- | ---------------------------------------------------------------------------
-- | CreateExperiment
-- | ---------------------------------------------------------------------------
data CreateExperiment = CreateExperiment
  { _ceProductId :: !Pid
  , _cePrice :: !Price
  , _ceName :: !Text
  , _ceCampaignId :: !CampaignId
  } deriving (Eq, Generic, Show)

makeLenses ''CreateExperiment

instance ToJSON CreateExperiment

instance FromJSON CreateExperiment

-- | ---------------------------------------------------------------------------
-- | ProductDetailsView
-- | ---------------------------------------------------------------------------
data ProductDetailsView = ProductDetailsView
  { expId :: !(Maybe ExpId)
  , page :: !Text -- product
  , pageUrl :: !Text
  , productId :: Int
  , userId :: !(Maybe Text)
  } deriving (Generic, Show)

instance FromJSON ProductDetailsView

instance ToJSON ProductDetailsView

-- | ---------------------------------------------------------------------------
-- | ProductListingsView
-- | ---------------------------------------------------------------------------
data ProductListingsView = ProductListingsView
  { expId :: !(Maybe ExpId)
  , page :: !Text -- collection
  , pageUrl :: !Text
  , productIds :: ![Int]
  , userId :: !(Maybe Text)
  } deriving (Generic, Show)

instance FromJSON ProductListingsView

instance ToJSON ProductListingsView

-- | ---------------------------------------------------------------------------
-- | CollectionListingsView
-- | ---------------------------------------------------------------------------
data CollectionListingsView = CollectionListingsView
  { expId :: !(Maybe ExpId)
  , page :: !Text -- collections
  , pageUrl :: !Text
  , userId :: !(Maybe Text)
  } deriving (Generic, Show)

instance FromJSON CollectionListingsView

instance ToJSON CollectionListingsView

-- | ---------------------------------------------------------------------------
-- | LineItem
-- | ---------------------------------------------------------------------------
data LineItem = LineItem
  { product_id :: !Int
  , variant_id :: !Int
  } deriving (Generic, Show)

instance FromJSON LineItem where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = quietSnake}

instance ToJSON LineItem

-- | ---------------------------------------------------------------------------
-- | CheckoutEvent
-- | ---------------------------------------------------------------------------
data CheckoutEvent = CheckoutEvent
  { lineItems :: ![LineItem]
  , page :: !Text -- checkout
  , pageUrl :: !Text
  , step :: !(Maybe Text)
  , token :: !(Maybe Text)
  , userId :: !(Maybe Text)
  } deriving (Generic, Show)

instance FromJSON CheckoutEvent

instance ToJSON CheckoutEvent

-- | ---------------------------------------------------------------------------
-- | UnknownView
-- | ---------------------------------------------------------------------------
data UnknownView = UnknownView
  { expId :: !(Maybe ExpId)
  , page :: !Text -- unknown
  , pageUrl :: !Text
  , userId :: !(Maybe Text)
  } deriving (Generic, Show)

instance FromJSON UnknownView

instance ToJSON UnknownView

-- | ---------------------------------------------------------------------------
-- | TrackView
-- | ---------------------------------------------------------------------------
data TrackView
  = Details ProductDetailsView
  | Listing ProductListingsView
  | Collection CollectionListingsView
  | Checkout CheckoutEvent
  | Unknown UnknownView
  deriving (Show)

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

-- | ---------------------------------------------------------------------------
-- | OkResponse
-- | ---------------------------------------------------------------------------
data OkResponse = OkResponse
  { message :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON OkResponse

eventTypeToText :: EventType -> Text
eventTypeToText View = "view"
