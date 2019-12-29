{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Data.Domain where

import Control.Lens.TH (makeLenses)
--import Data.Aeson (ToJSON, FromJSON(..), withObject, (.:))
import Data.Text (Text)
import Data.Time (LocalTime)
import GHC.Generics (Generic)
import SweetSpot.Data.Common

-- | ---------------------------------------------------------------------------
-- | CheckoutEvent
-- | ---------------------------------------------------------------------------
-- data CheckoutEvent = CheckoutEvent
--   { _chkId :: !EventId
--   , _chkUserId :: !UserId
--   , _chkBucketId :: !BucketId
--   , _chkOrderId :: !OrderId
--   , _chkTimestamp :: !LocalTime
--   , _chkLineItems :: ![LineItem]
--   } deriving (Eq, Generic, Show)

-- makeLenses ''CheckoutEvent

-- instance ToJSON CheckoutEvent

-- | ---------------------------------------------------------------------------
-- | Campaign
-- | ---------------------------------------------------------------------------
data Campaign = Campaign
  { _cCampaignId :: !CampaignId
  , _cCampaignName :: !Text
  , _cMinProfitIncrease :: !Int
  , _cStartDate :: !LocalTime
  , _cEndDate :: !LocalTime
  } deriving (Eq, Generic, Show)

makeLenses ''Campaign

-- | ---------------------------------------------------------------------------
-- | DBBucketStats
-- | ---------------------------------------------------------------------------
-- data DBBucketStats = DBBucketStats
--   { _dbsBucketId :: !BucketId
--   , _dbsBucketType :: !BucketType
--   , _dbsOriginalSvid :: !Svid
--   , _dbsTestSvid :: !Svid
--   , _dbsUserCount :: !Int
--   , _dbsImpressionCount :: !Int
--   , _dbsCheckoutEvents :: ![CheckoutEvent]
--   , _dbsPrice :: !Price
--   } deriving (Eq, Generic, Show)

-- makeLenses ''DBBucketStats

-- | ---------------------------------------------------------------------------
-- | DBExperimentStats
-- | ---------------------------------------------------------------------------
-- data DBExperimentStats = DBExperimentStats
--   { _desExpId :: !ExpId
--   , _desProductName :: !Text
--   , _desBuckets :: ![DBBucketStats]
--   } deriving (Eq, Generic, Show)

-- makeLenses ''DBExperimentStats

-- | ---------------------------------------------------------------------------
-- | DBCampaignStats
-- | ---------------------------------------------------------------------------
-- data DBCampaignStats = DBCampaignStats
--   { _dcsCampaignId :: !CampaignId
--   , _dcsCampaignName :: !Text
--   , _dcsMinProfitIncrease :: !Int
--   , _dcsStartDate :: !LocalTime
--   , _dcsEndDate :: !LocalTime
--   , _dcsExperiments :: ![DBExperimentStats]
--   } deriving (Eq, Generic, Show)

-- makeLenses ''DBCampaignStats
