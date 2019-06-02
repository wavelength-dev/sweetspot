module Supple.Calc (enhanceDBStats) where

import Control.Lens
import qualified Data.List as L
import Data.Maybe (isJust, fromJust)
import Supple.Data.Common (BucketType(..))
import Supple.Data.Domain
import Supple.Data.Api


conversionRate :: Int -> Int -> Double
conversionRate conversions users =
  (fromIntegral conversions) / (fromIntegral users)

enhanceDBBucketStats :: DBBucketStats -> BucketStats
enhanceDBBucketStats stats =
  let testVariantId = stats ^. dbsTestSvid
      userCount = stats ^. dbsUserCount
      -- | TODO Maybe ping shopify to check fulfillment status
      conversionCount =
        stats ^. dbsCheckoutEvents
          & filter (\e -> e ^. chkLineItems & L.find (== testVariantId) & isJust)
          & length
      conversion = conversionRate conversionCount userCount
   in BucketStats
        { _bsBucketId = stats ^. dbsBucketId
        , _bsBucketType = stats ^. dbsBucketType
        , _bsUserCount = userCount
        , _bsImpressionCount = stats ^. dbsImpressionCount
        , _bsConversionCount = conversionCount
        , _bsConversionRate = conversion
        }

enhanceDBExperimentStats :: DBExperimentStats -> ExperimentStats
enhanceDBExperimentStats stats =
  let buckets = stats ^. desBuckets
      totalUserCount = buckets ^.. traverse . dbsUserCount & sum
      totalImpressionCount = buckets ^.. traverse . dbsImpressionCount & sum
      enhancedBucketStats = enhanceDBBucketStats <$> stats ^. desBuckets
      totalConversionCount =
        enhancedBucketStats & fmap (^. bsConversionCount) & sum

      findType :: BucketType -> BucketStats
      findType = \t -> fromJust $ L.find ((== t) . (^. bsBucketType)) enhancedBucketStats

      -- Assumes one control and test respectively
      control = findType Control
      test = findType Test

  in ExperimentStats
        { _esExpId = stats ^. desExpId
        , _esUserCount = totalUserCount
        , _esImpressionCount = totalImpressionCount
        , _esConversionCount = totalConversionCount
        , _esConversionRate =
          conversionRate totalConversionCount totalUserCount
        , _esBuckets = enhancedBucketStats
        }

enhanceDBStats :: DBCampaignStats -> CampaignStats
enhanceDBStats stats =
    CampaignStats
        { _csCampaignId = stats ^. dcsCampaignId
        , _csCampaignName = stats ^. dcsCampaignName
        , _csMinProfitIncrease = stats ^. dcsMinProfitIncrease
        , _csStartDate = stats ^. dcsStartDate
        , _csEndDate = stats ^. dcsEndDate
        , _csExperiments = enhancedExperiments
        }
  where enhancedExperiments =
          stats ^. dcsExperiments ^.. traverse & fmap enhanceDBExperimentStats
