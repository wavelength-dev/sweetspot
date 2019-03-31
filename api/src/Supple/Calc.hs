module Supple.Calc (enhanceDBStats) where

import Control.Lens
import qualified Data.List as L
import Data.Maybe (isJust, fromJust)
import Data.Number.Erf (inverf)
import Statistics.ConfidenceInt (binomialCI)
import Statistics.Types
import Statistics.Distribution (complCumulative, quantile)
import Statistics.Distribution.Normal (normalDistr)
import Supple.Data.Common (BucketType(..))
import Supple.Data.Domain
import Supple.Data.Api


binomialSampleSize :: Double -> Int
binomialSampleSize sampleProp =
  round
    (2 * sampleProp * (1 - sampleProp) *
    (inverf confidenceLvl / marginOfError) ^ 2)
  where
    marginOfError = 0.0025
    confidenceLvl = 0.95

-- p = sample proportion, n = sample size
variance :: Double -> Int -> Double
variance p n = p * (1 - p) / fromIntegral n

standardDeviation :: Double -> Int -> Double
standardDeviation p n = sqrt $ variance p n

-- Get ratio between test/control conversion rates needed for
-- given improvement in profit
conversionRatio :: Double -> Double -> Double -> Double
conversionRatio marginTest marginControl profitImprovement =
  marginControl * profitImprovement / marginTest

conversionRate :: Int -> Int -> Double
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
        , _bsBucketType = stats ^. dbsBucketType
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

      findType :: BucketType -> BucketStats
      findType = \t -> fromJust $ L.find ((== t) . (^. bsBucketType)) enhancedBucketStats

      -- Assumes one control and test respectively
      control = findType Control
      test = findType Test
      -- TODO: add actual margin data
      ratio = conversionRatio 100 80 (stats ^. desMinProfitIncrease & fromIntegral & (/ 100))
      targetCR = (test ^. bsConversionRate) * ratio
      cVariance = variance (control ^. bsConversionRate) (control ^. bsUserCount)
      tVariance = variance (test ^. bsConversionRate) (test ^. bsUserCount)
      diffMean = (control ^. bsConversionRate) - (test ^. bsConversionRate)
      diffStdDev = sqrt (cVariance + tVariance)
      diffNormalDistr = normalDistr diffMean diffStdDev
      pMinCR = complCumulative diffNormalDistr targetCR
      crP95 = quantile diffNormalDistr 0.95


   in ExperimentStats
        { _esExpId = stats ^. desExpId
        , _esUserCount = totalUserCount
        , _esImpressionCount = totalImpressionCount
        , _esConversionCount = totalConversionCount
        , _esConversionRate =
          conversionRate totalConversionCount totalUserCount
        , _esPMinCR = pMinCR
        , _esCRp95 = crP95
        , _esBuckets = enhancedBucketStats
        }
