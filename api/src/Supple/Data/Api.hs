{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Supple.Data.Api where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Supple.Data.Common
import Supple.Data.Domain (CheckoutEvent)

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
  , _eCampaignId :: !CampaignId
  } deriving (Eq, Generic, Show)

makeLenses ''Experiment

-- | ---------------------------------------------------------------------------
-- | ExperimentBuckets
-- | ---------------------------------------------------------------------------
data ExperimentBuckets = ExperimentBuckets
  { _ebExpId :: !ExpId
  , _ebSku :: !Sku
  , _ebName :: !Text
  , _ebCampaignId :: !CampaignId
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
  , _ubBucketId :: !BucketId
  } deriving (Eq, Generic, Show)

makeLenses ''UserBucket

instance ToJSON UserBucket

-- | ---------------------------------------------------------------------------
-- | BucketStats
-- | ---------------------------------------------------------------------------
data BucketStats = BucketStats
  { _bsBucketId :: !BucketId
  , _bsUserCount :: !Int
  , _bsImpressionCount :: !Int
  , _bsConversionCount :: !Int
  , _bsCheckoutEvents :: ![CheckoutEvent]
  } deriving (Eq, Generic, Show)

makeLenses ''BucketStats

instance ToJSON BucketStats

-- | ---------------------------------------------------------------------------
-- | ExperimentStats
-- | ---------------------------------------------------------------------------
data ExperimentStats = ExperimentStats
  { _esExpId :: !ExpId
  , _esUserCount :: !Int
  , _esImpressionCount :: !Int
  , _esBuckets :: ![BucketStats]
  } deriving (Eq, Generic, Show)

instance ToJSON ExperimentStats

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
-- | OkResponse
-- | ---------------------------------------------------------------------------
data OkResponse = OkResponse
  { message :: !Text
  } deriving (Eq, Generic, Show)

instance ToJSON OkResponse

eventTypeToText :: EventType -> Text
eventTypeToText Checkout = "checkout"
eventTypeToText Log = "log"
eventTypeToText View = "view"
