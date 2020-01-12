{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module SweetSpot.Data.Api where

import           Control.Lens.TH                ( makeLenses )
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , object
                                                , withObject
                                                , (.=)
                                                , (.:)
                                                )
import           Data.Aeson.Types               ( parse )
import           Data.Scientific                ( Scientific )
import           Data.Text                      ( Text )

import           GHC.Generics                   ( Generic )
import           SweetSpot.Data.Common
import           SweetSpot.Shopify.Types        ( FromShopJSON(..)
                                                , ToShopJSON(..)
                                                )

-- | ---------------------------------------------------------------------------
-- | Image
-- | ---------------------------------------------------------------------------
data Image = Image
  { _imageSrc :: !Text
  } deriving (Eq, Generic, Show)

makeLenses ''Image

instance FromJSON Image

instance ToJSON Image

instance FromShopJSON Image where
  parseShopJSON = withObject "Image" $ \v -> Image
    <$> v .: "src"

-- | ---------------------------------------------------------------------------
-- | Variant
-- | ---------------------------------------------------------------------------
data Variant = Variant
  { _variantId :: !Svid
  , _variantProductId :: !Pid
  , _variantTitle :: !Text
  , _variantSku :: !Sku
  , _variantPrice :: !Price
  } deriving (Eq, Generic, Show)

makeLenses ''Variant

instance ToJSON Variant

instance FromJSON Variant

instance FromShopJSON Variant where
  parseShopJSON = withObject "Variant" $ \v -> do
    id <- v .: "id"
    productId <- v .: "product_id"
    title <- v .: "title"
    sku <- v .: "sku"
    price <- v .: "price"
    return Variant
      { _variantId = Svid .  showText @Int $ id
      , _variantProductId = Pid . showText @Int $ productId
      , _variantTitle = title
      , _variantSku = sku
      , _variantPrice = Price . read @Scientific $ price
      }

-- | ---------------------------------------------------------------------------
-- | Product
-- | ---------------------------------------------------------------------------
data Product = Product
  { _productId :: !Pid
  , _productTitle :: !Text
  , _productVariants :: ![Variant]
  , _productImage :: !Image
  } deriving (Eq, Generic, Show)

makeLenses ''Product

instance ToJSON Product

instance FromJSON Product

instance FromShopJSON Product where
  parseShopJSON = withObject "Product" $ \v -> do
    id <- v .: "id"
    title <- v .: "title"
    variants <- v .: "variants" >>= traverse parseShopJSON
    image <- v .: "image" >>= parseShopJSON
    return Product
      { _productId = Pid . showText @Int $ id
      , _productTitle = title
      , _productVariants = variants
      , _productImage = image
      }

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
  , _ceShopDomain :: !ShopDomain
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
  } deriving (Eq, Generic, Show)

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

instance FromJSON LineItem


-- | ---------------------------------------------------------------------------
-- | ApiCheckoutEvent
-- | ---------------------------------------------------------------------------
data ApiCheckoutEvent = ApiCheckoutEvent
  { _aceCampaignId :: CampaignId
  , _aceOrderId :: OrderId
  , _aceUserId :: UserId
  , _aceItems :: [LineItem]
  } deriving (Generic, Show)

makeLenses ''ApiCheckoutEvent

instance ToJSON ApiCheckoutEvent

instance FromJSON ApiCheckoutEvent
