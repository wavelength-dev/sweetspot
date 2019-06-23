{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module SweetSpot.Data.Domain where

import Control.Lens.TH (makeLenses)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import SweetSpot.Data.Common

-- | ---------------------------------------------------------------------------
-- | CheckoutEvent
-- | ---------------------------------------------------------------------------
data CheckoutEvent = CheckoutEvent
  { _chkId :: !EventId
  , _chkUserId :: !UserId
  , _chkBucketId :: !BucketId
  , _chkOrderId :: !OrderId
  , _chkTimestamp :: !UTCTime
  , _chkLineItems :: ![Svid]
  } deriving (Eq, Generic, Show)

makeLenses ''CheckoutEvent

instance ToJSON CheckoutEvent

-- | ---------------------------------------------------------------------------
-- | Campaign
-- | ---------------------------------------------------------------------------
data Campaign = Campaign
  { _cCampaignId :: !CampaignId
  , _cCampaignName :: !Text
  , _cMinProfitIncrease :: !Int
  , _cStartDate :: !(Maybe UTCTime)
  , _cEndDate :: !(Maybe UTCTime)
  } deriving (Eq, Generic, Show)

makeLenses ''Campaign

-- | ---------------------------------------------------------------------------
-- | DBBucketStats
-- | ---------------------------------------------------------------------------
data DBBucketStats = DBBucketStats
  { _dbsBucketId :: !BucketId
  , _dbsBucketType :: !BucketType
  , _dbsOriginalSvid :: !Svid
  , _dbsTestSvid :: !Svid
  , _dbsUserCount :: !Int
  , _dbsImpressionCount :: !Int
  , _dbsCheckoutEvents :: ![CheckoutEvent]
  , _dbsPrice :: !Price
  , _dbsCost :: !Price
  } deriving (Eq, Generic, Show)

makeLenses ''DBBucketStats

-- | ---------------------------------------------------------------------------
-- | DBExperimentStats
-- | ---------------------------------------------------------------------------
data DBExperimentStats = DBExperimentStats
  { _desExpId :: !ExpId
  , _desProductName :: !Text
  , _desBuckets :: ![DBBucketStats]
  } deriving (Eq, Generic, Show)

makeLenses ''DBExperimentStats

-- | ---------------------------------------------------------------------------
-- | DBCampaignStats
-- | ---------------------------------------------------------------------------
data DBCampaignStats = DBCampaignStats
  { _dcsCampaignId :: !CampaignId
  , _dcsCampaignName :: !Text
  , _dcsMinProfitIncrease :: !Int
  , _dcsStartDate :: !(Maybe UTCTime)
  , _dcsEndDate :: !(Maybe UTCTime)
  , _dcsExperiments :: ![DBExperimentStats]
  } deriving (Eq, Generic, Show)

makeLenses ''DBCampaignStats
