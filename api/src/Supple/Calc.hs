module Supple.Calc (enhanceDBStats) where

import Control.Lens
import qualified Data.List as L
import Data.Maybe (isJust)
import Data.Number.Erf (inverf)
import Statistics.ConfidenceInt (binomialCI)
import Statistics.Types
import qualified Statistics.Distribution as D
import Statistics.Distribution.Normal
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

-- p = sample proportion, n = sample size
variance :: Float -> Int -> Float
variance p n = p * (1 - p) / fromIntegral n

standardDeviation :: Float -> Int -> Float
standardDeviation p n = sqrt $ variance p n

-- Get ratio between test/control conversion rates needed for
-- given improvement in profit
conversionRatio :: Float -> Float -> Float -> Float
conversionRatio marginTest marginControl profitImprovement =
  marginControl * profitImprovement / marginTest

conversionRate :: Int -> Int -> Float
conversionRate conversions users =
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
      conversion = conversionRate conversionCount userCount
   in BucketStats
        { _bsBucketId = stats ^. dbsBucketId
        , _bsUserCount = userCount
        , _bsImpressionCount = stats ^. dbsImpressionCount
        , _bsConversionCount = conversionCount
        , _bsConversionRate = conversion
        , _bsConfidenceInterval = binomialCI cl95 userCount conversionCount
        , _bsEstSamplesToSig = userCount - binomialSampleSize conversion
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
          conversionRate totalConversionCount totalUserCount
        , _esBuckets = enhancedBucketStats
        }
