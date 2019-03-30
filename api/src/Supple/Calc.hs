module Supple.Calc (enhanceDBStats) where

import Control.Lens
import qualified Data.List as L
import Data.Maybe (isJust)
import Data.Number.Erf (inverf)
import Statistics.ConfidenceInt (binomialCI)
import Statistics.Types
import Supple.Data.Domain
import Supple.Data.Api


binomialSampleSize :: Float -> Int
binomialSampleSize sampleProp =
  round
    (2 * sampleProp * (1 - sampleProp) *
    (inverf confidenceLvl / marginOfError) ^ 2)
  where
    marginOfError = 0.0025
    confidenceLvl = 0.95

-- Get ratio between test/control conversion rates needed for
-- given improvement in profit
getConversionRatio :: Float -> Float -> Float -> Float
getConversionRatio marginTest marginControl profitImprovement =
  marginControl * profitImprovement / marginTest

calculateConversionRate :: Int -> Int -> Float
calculateConversionRate conversions users =
  (fromIntegral conversions) / (fromIntegral users)

enhanceDBBucketStats :: DBBucketStats -> BucketStats
enhanceDBBucketStats stats =
  let variantId = stats ^. dbsSvid
      userCount = stats ^. dbsUserCount
    -- | TODO Maybe ping shopify to check fulfillment status
      conversionCount =
        stats ^. dbsCheckoutEvents
          & filter (\e -> e ^. chkLineItems & L.find (== variantId) & isJust)
          & length
      conversionRate = calculateConversionRate conversionCount userCount
   in BucketStats
        { _bsBucketId = stats ^. dbsBucketId
        , _bsUserCount = userCount
        , _bsImpressionCount = stats ^. dbsImpressionCount
        , _bsConversionCount = conversionCount
        , _bsConversionRate = conversionRate
        , _bsConfidenceInterval = binomialCI cl95 userCount conversionCount
        , _bsEstSamplesToSig = userCount - binomialSampleSize conversionRate
        }

enhanceDBStats :: DBExperimentStats -> ExperimentStats
enhanceDBStats stats =
  let buckets = stats ^. desBuckets
      totalUserCount = buckets ^.. traverse . dbsUserCount & sum
      totalImpressionCount = buckets ^.. traverse . dbsImpressionCount & sum
      enhancedBucketStats = enhanceDBBucketStats <$> stats ^. desBuckets
      totalConversionCount =
        enhancedBucketStats & fmap (^. bsConversionCount) & sum
   in ExperimentStats
        { _esExpId = stats ^. desExpId
        , _esUserCount = totalUserCount
        , _esImpressionCount = totalImpressionCount
        , _esConversionCount = totalConversionCount
        , _esConversionRate =
          calculateConversionRate totalConversionCount totalUserCount
        , _esBuckets = enhancedBucketStats
        }
