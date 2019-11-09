{-# LANGUAGE FlexibleContexts #-}

module SweetSpot.Calc (enhanceDBStats) where

import Control.Lens
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import qualified Data.List as L
import Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Scientific (toRealFloat)
import qualified Data.Vector.Unboxed as V
import Statistics.Types (cl95, Estimate, ConfInt, cl90)
import Statistics.Resampling (Estimator(..), resample)
import Statistics.Resampling.Bootstrap (bootstrapBCA)
import System.Random.MWC (createSystemRandom, GenIO)
import SweetSpot.AppM
import SweetSpot.Data.Common (Svid, BucketType(..), Price(..), UserId)
import SweetSpot.Data.Domain
import SweetSpot.Data.Api

conversionRate :: Int -> Int -> Double
conversionRate conversions users =
  fromIntegral conversions / fromIntegral users

findType :: BucketType -> [BucketStats] -> BucketStats
findType t stats = fromJust $ L.find ((== t) . (^. bsBucketType)) stats

instance Semigroup Double where
  (<>) = (+)

-- Merge multiple checkouts for same user
groupByUser :: Semigroup a => [(UserId, a)] -> [(UserId, a)]
groupByUser =  M.toList . M.fromListWith (<>)

-- Get actual revenue from LineItems
calcRev :: Svid -> Double -> [(UserId, [LineItem])] -> [(UserId, Double)]
calcRev svid p = mapped . _2 %~ revenue . getMatchingPurchases
  where
    getMatchingPurchases = L.filter ((== svid) . (^. liVariantId))
    revenue lis =
        lis ^.. traverse . liQuantity
          & sum
          & \qty -> fromIntegral qty * p

sampleForBucketType :: BucketType -> [BucketStats] -> [Double]
sampleForBucketType t bs =
  L.filter ((== t) . (^. bsBucketType)) bs
    & (>>= (^. bsUserRevenues))
    & groupByUser
    & fmap snd

enhanceDBBucketStats :: DBBucketStats -> BucketStats
enhanceDBBucketStats stats =
    BucketStats
        { _bsBucketId = stats ^. dbsBucketId
        , _bsBucketType = stats ^. dbsBucketType
        , _bsUserCount = stats ^. dbsUserCount
        , _bsImpressionCount = stats ^. dbsImpressionCount
        , _bsPrice = stats ^. dbsPrice
        , _bsUserRevenues = userRevenues
        }
    where
      (Price p) = stats ^. dbsPrice
      testVariantId = stats ^. dbsTestSvid
      -- | TODO Maybe ping shopify to check fulfillment status
      userRevenues :: [(UserId, Double)]
      userRevenues = stats ^. dbsCheckoutEvents
        & fmap (\ev -> (ev ^. chkUserId, ev ^. chkLineItems))
        & groupByUser
        & calcRev testVariantId (toRealFloat p)
        & L.filter ((> 0) . snd)

enhanceDBExperimentStats :: DBExperimentStats -> ExperimentStats
enhanceDBExperimentStats stats =
  let buckets = stats ^. desBuckets
      totalUserCount = buckets ^.. traverse . dbsUserCount & sum
      totalImpressionCount = buckets ^.. traverse . dbsImpressionCount & sum
      enhancedBucketStats = enhanceDBBucketStats <$> stats ^. desBuckets
      -- Assumes one control and test respectively
      control = findType Control enhancedBucketStats
      test = findType Test enhancedBucketStats

  in ExperimentStats
        { _esExpId = stats ^. desExpId
        , _esUserCount = totalUserCount
        , _esImpressionCount = totalImpressionCount
        , _esBuckets = enhancedBucketStats
        }

enhanceDBStats
  :: (MonadIO m, MonadReader AppCtx m)
  => DBCampaignStats
  -> m CampaignStats
enhanceDBStats stats = do
    gen <- liftIO createSystemRandom
    controlEstimate <- liftIO $ bootstrap gen controlSample
    testEstimate <- liftIO $ bootstrap gen testSample
    return CampaignStats
        { _csCampaignId = stats ^. dcsCampaignId
        , _csCampaignName = stats ^. dcsCampaignName
        , _csMinProfitIncrease = stats ^. dcsMinProfitIncrease
        , _csStartDate = stats ^. dcsStartDate
        , _csEndDate = stats ^. dcsEndDate
        , _csExperiments = []
        , _csProfitPerUserControl = controlEstimate
        , _csProfitPerUserTest = testEstimate
        , _csConvertersControl = convertersControl
        , _csNonConvertersControl = nonConvertersCountControl
        , _csConvertersTest = convertersTest
        , _csNonConvertersTest = nonConvertersCountTest
        , _csConvertersControlCount = convertersCountControl
        , _csConvertersTestCount = convertersCountTest
        , _csConversionRateControl =
            conversionRate convertersCountControl (convertersCountControl + nonConvertersCountControl)
        , _csConversionRateTest =
            conversionRate convertersCountTest (convertersCountTest + nonConvertersCountTest)
        }
  where
    enhancedExperiments =
      stats ^. dcsExperiments ^.. traverse & fmap enhanceDBExperimentStats
    bucketStats =
      enhancedExperiments ^.. traverse . esBuckets & mconcat

    convertersControl = sampleForBucketType Control bucketStats
    convertersTest = sampleForBucketType Test bucketStats

    nonConvertersControl = nonConvertersForBucketType Control bucketStats
    nonConvertersTest = nonConvertersForBucketType Test bucketStats

    convertersCountControl = L.length convertersControl
    convertersCountTest = L.length convertersTest

    nonConvertersCountControl = L.length nonConvertersControl
    nonConvertersCountTest = L.length nonConvertersTest

    controlSample = V.fromList $ convertersControl <> nonConvertersControl
    testSample = V.fromList $ convertersTest <> nonConvertersTest

    nonConvertersForBucketType :: BucketType -> [BucketStats] -> [Double]
    nonConvertersForBucketType t bs =
      bs
        & L.filter ((== t) . (^. bsBucketType))
        & ((^. bsUserCount) . (!! 0))
        & flip (-) convs
        & flip L.take (repeat 0.0)
      where
        convs = case t of
          Control -> L.length convertersControl
          Test -> L.length convertersTest

bootstrap :: GenIO -> V.Vector Double -> IO (Estimate ConfInt Double)
bootstrap gen sample = do
  resampled <- resample gen [Mean] 1000 sample
  return $ head $ bootstrapBCA cl90 sample resampled



-- | ---------------------------------------------------------------------------
-- | Simulation
-- | ---------------------------------------------------------------------------
data SimParams = SimParams
  { _size :: Int
  , _cr :: Double
  , _avgUsrProfit :: Double
  }

genSample :: SimParams -> V.Vector Double
genSample params = convs <> nonConvs
  where
    convCount = round (_cr params * fromIntegral (_size params))
    nonConvCount = _size params - convCount
    convs = V.fromList $ L.replicate convCount (_avgUsrProfit params)
    nonConvs = V.fromList $ L.replicate nonConvCount 0.0

simulate :: SimParams
         -> SimParams
         -> IO ()
simulate c t = do
  gen <- createSystemRandom
  controlRes <- bootstrap gen $ genSample c
  testRes <- bootstrap gen $ genSample t
  print controlRes
  putStrLn "==="
  print testRes
  return ()
