{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Supple.Data.Domain where

import Control.Lens.TH (makeLenses)
import Data.Aeson (ToJSON)
import Control.Lens
import Data.Maybe (isJust)
import qualified Data.List as L
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Supple.Data.Api
import Supple.Data.Common

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
-- | DBBucketStats
-- | ---------------------------------------------------------------------------
data DBBucketStats = DBBucketStats
  { _dbsBucketId :: !BucketId
  , _dbsSvid :: !Svid
  , _dbsUserCount :: !Int
  , _dbsImpressionCount :: !Int
  , _dbsCheckoutEvents :: ![CheckoutEvent]
  } deriving (Eq, Generic, Show)

makeLenses ''DBBucketStats

-- | ---------------------------------------------------------------------------
-- | DBExperimentStats
-- | ---------------------------------------------------------------------------
data DBExperimentStats = DBExperimentStats
  { _desExpId :: !ExpId
  , _desBuckets :: ![DBBucketStats]
  } deriving (Eq, Generic, Show)

makeLenses ''DBExperimentStats

-- | ---------------------------------------------------------------------------
-- | Transformations
-- | ---------------------------------------------------------------------------
enhanceDBBucketStats :: DBBucketStats -> BucketStats
enhanceDBBucketStats stats =
  let variantId = stats ^. dbsSvid
      userCount = stats ^. dbsUserCount
    -- | TODO Maybe ping shopify to check fulfillment status
      conversionCount =
        stats ^. dbsCheckoutEvents
          & filter (\e -> e ^. chkLineItems & L.find (== variantId) & isJust)
          & length
   in BucketStats
        { _bsBucketId = stats ^. dbsBucketId
        , _bsUserCount = userCount
        , _bsImpressionCount = stats ^. dbsImpressionCount
        , _bsConversionCount = conversionCount
        , _bsConversionRate =
          (fromIntegral conversionCount) / (fromIntegral userCount) & (* 100)
        }

enhanceDBStats :: DBExperimentStats -> ExperimentStats
enhanceDBStats stats =
  let buckets = stats ^. desBuckets
      totalUserCount = buckets ^.. traverse . dbsUserCount & sum
      totalImpressionCount = buckets ^.. traverse . dbsUserCount & sum
      enhancedBucketStats = enhanceDBBucketStats <$> stats ^. desBuckets
      totalConversionCount =
        enhancedBucketStats & fmap (^. bsConversionCount) & sum
   in ExperimentStats
        { _esExpId = stats ^. desExpId
        , _esUserCount = totalUserCount
        , _esImpressionCount = totalImpressionCount
        , _esConversionCount = totalConversionCount
        , _esBuckets = enhancedBucketStats
        }
