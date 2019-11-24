{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module SweetSpot.Data.Api where

import           Control.Lens.TH                ( makeLenses )
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , object
                                                , withObject
                                                , (.=)
                                                , (.:)
                                                )
import           Statistics.Types               ( Estimate(..)
                                                , ConfInt(..)
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time                      ( LocalTime )
import           GHC.Generics                   ( Generic )
import           SweetSpot.Data.Common

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
  , _vPrice :: !Price
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
-- data Bucket = Bucket
--   { _bBucketId :: !BucketId
--   , _bBucketType :: !BucketType
--   , _bOriginalSvid :: !Svid
--   , _bTestSvid :: !Svid
--   , _bPrice :: !Price
--   , _bControlPrice :: !Price
--   } deriving (Eq, Generic, Show)

-- makeLenses ''Bucket

-- instance ToJSON Bucket

-- instance FromJSON Bucket

-- | ---------------------------------------------------------------------------
-- | Experiment
-- | ---------------------------------------------------------------------------
-- data Experiment = Experiment
--   { _eExpId :: !ExpId
--   , _eSku :: !Sku
--   , _eProductName :: !Text
--   } deriving (Eq, Generic, Show)

-- makeLenses ''Experiment

-- | ---------------------------------------------------------------------------
-- | ExperimentBuckets
-- | ---------------------------------------------------------------------------
-- data ExperimentBuckets = ExperimentBuckets
--   { _ebExpId :: !ExpId
--   , _ebSku :: !Sku
--   , _ebProductName :: !Text
--   , _ebBuckets :: ![Bucket]
--   } deriving (Eq, Generic, Show)

-- makeLenses ''ExperimentBuckets

-- instance ToJSON ExperimentBuckets

-- instance FromJSON ExperimentBuckets

-- | ---------------------------------------------------------------------------
-- | UserBucket
-- | ---------------------------------------------------------------------------
-- data UserBucket = UserBucket
--   { _ubUserId :: !UserId
--   , _ubSku :: !Sku
--   , _ubOriginalSvid :: !Svid
--   , _ubTestSvid :: !Svid
--   , _ubPrice :: !Price
--   , _ubExpId :: !ExpId
--   , _ubBucketId :: !BucketId
--   , _ubBucketType :: !BucketType
--   , _ubControlPrice :: !Price
--   } deriving (Eq, Generic, Show)

-- makeLenses ''UserBucket

-- instance ToJSON UserBucket

-- instance FromJSON UserBucket

-- | ---------------------------------------------------------------------------
-- | BucketStats
-- | ---------------------------------------------------------------------------
-- data BucketStats = BucketStats
--   { _bsBucketId :: !BucketId
--   , _bsBucketType :: !BucketType
--   , _bsUserCount :: !Int
--   , _bsImpressionCount :: !Int
--   , _bsPrice :: !Price
--   , _bsUserRevenues :: ![(UserId, Double)]
--   } deriving (Eq, Generic, Show)

-- makeLenses ''BucketStats

-- instance ToJSON BucketStats

-- instance FromJSON BucketStats

-- | ---------------------------------------------------------------------------
-- | ExperimentStats
-- | ---------------------------------------------------------------------------
-- data ExperimentStats = ExperimentStats
--   { _esExpId :: !ExpId
--   , _esUserCount :: !Int
--   , _esImpressionCount :: !Int
--   , _esBuckets :: ![BucketStats]
--   } deriving (Eq, Generic, Show)

-- makeLenses ''ExperimentStats

-- instance ToJSON ExperimentStats

-- instance FromJSON ExperimentStats

-- | ---------------------------------------------------------------------------
-- | CampaignStats
-- | ---------------------------------------------------------------------------
-- data CampaignStats = CampaignStats
--   { _csCampaignId :: !CampaignId
--   , _csCampaignName :: !Text
--   , _csMinProfitIncrease :: !Int
--   , _csStartDate :: !LocalTime
--   , _csEndDate :: !LocalTime
--   , _csExperiments :: ![ExperimentStats]
--   , _csProfitPerUserControl :: !(Estimate ConfInt Double)
--   , _csProfitPerUserTest :: !(Estimate ConfInt Double)
--   , _csConvertersControl :: ![Double]
--   , _csNonConvertersControl :: !Int
--   , _csConvertersTest :: ![Double]
--   , _csNonConvertersTest :: !Int
--   , _csConvertersControlCount :: !Int
--   , _csConvertersTestCount :: !Int
--   , _csConversionRateControl :: !Double
--   , _csConversionRateTest :: !Double
--   } deriving (Eq, Generic, Show)

-- makeLenses ''CampaignStats

-- instance ToJSON CampaignStats

-- instance FromJSON CampaignStats

-- | ---------------------------------------------------------------------------
-- | CreateExperiment
-- | ---------------------------------------------------------------------------
data CreateExperiment = CreateExperiment
  { _ceProductId :: !Pid
  , _cePrice :: !Price
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

instance FromJSON OkResponse

-- | ---------------------------------------------------------------------------
-- | TestMap
-- | ---------------------------------------------------------------------------
data TestMap = TestMap
  { userId :: !UserId
  , targetId :: !Svid
  , sku :: !Sku
  , swapId :: !Svid
  , swapPrice :: !Price
  } deriving (Eq, Generic)

instance ToJSON TestMap where
        toJSON (TestMap (UserId userId) (Svid targetId) sku (Svid swapId) price)
                = object
                        [ "userId" .= userId
                        , "targetId" .= targetId
                        , "sku" .= sku
                        , "swapId" .= swapId
                        , "swapPrice" .= price
                        ]

instance FromJSON TestMap where
        parseJSON = withObject "TestMap" $ \v -> do
                userId    <- v .: "userId"
                targetId  <- v .: "targetId"
                sku       <- v .: "sku"
                swapId    <- v .: "swapId"
                swapPrice <- v .: "swapPrice"

                return TestMap { userId    = UserId userId
                               , targetId  = Svid targetId
                               , sku       = Sku sku
                               , swapId    = Svid swapId
                               , swapPrice = Price swapPrice
                               }

-- | ---------------------------------------------------------------------------
-- | LineItem
-- | ---------------------------------------------------------------------------
data LineItem = LineItem
  { _liProductId :: !Pid
  , _liVariantId :: !Svid
  , _liSku :: !Sku
  , _liQuantity :: !Int
  } deriving (Eq, Generic, Show)

makeLenses ''LineItem

instance ToJSON LineItem

instance FromJSON LineItem where
        parseJSON = withObject "LineItem" $ \o -> do
                pid      <- o .: "productId"
                svid     <- o .: "variantId"
                sku      <- o .: "sku"
                quantity <- o .: "quantity"
                return LineItem
                        { _liProductId = Pid $ T.pack . show $ (pid :: Int)
                        , _liVariantId = Svid $ T.pack . show $ (svid :: Int)
                        , _liSku       = Sku sku
                        , _liQuantity  = quantity
                        }


-- | ---------------------------------------------------------------------------
-- | ApiCheckoutEvent
-- | ---------------------------------------------------------------------------
data ApiCheckoutEvent = ApiCheckoutEvent
  { _aceCampaignId :: CampaignId
  , _aceOrderId :: OrderId
  , _aceShopId :: ShopId
  , _aceUserId :: UserId
  , _aceItems :: [LineItem]
  } deriving (Generic, Show)

makeLenses ''ApiCheckoutEvent

instance ToJSON ApiCheckoutEvent

instance FromJSON ApiCheckoutEvent
