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
import           Data.Scientific                ( Scientific )
import           Data.Text                      ( Text )
import           Data.Time                      ( LocalTime )
import           GHC.Generics                   ( Generic )
import           SweetSpot.Data.Common
import           SweetSpot.Shopify.Types        ( FromShopJSON(..) )

-- | ---------------------------------------------------------------------------
-- | Image
-- | ---------------------------------------------------------------------------
data Image = Image
  { _imageSrc :: !Text
  } deriving (Eq, Generic, Show)

makeLenses ''Image

instance FromJSON Image

instance ToJSON Image where
  toJSON img = object
    [ "tag" .=  ("Image" :: Text)
    , "values" .= [
        object [ "_imageSrc" .= _imageSrc img ]
      ]
    ]

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

instance ToJSON Variant where
  toJSON var = object
    [ "tag" .=  ("Variant" :: Text)
    , "values" .= [
        object [ "_variantId" .= _variantId var
               , "_variantProductId" .= _variantProductId var
               , "_variantTitle" .= _variantTitle var
               , "_variantSku" .= _variantSku var
               , "_variantPrice" .= _variantPrice var
               ]
      ]
    ]

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

instance ToJSON Product where
  toJSON p = object
    [ "tag" .=  ("Product" :: Text)
    , "values" .= [
        object [ "_productId" .= _productId p
               , "_productTitle" .= _productTitle p
               , "_productVariants" .= _productVariants p
               , "_productImage" .= _productImage p
               ]
      ]
    ]

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
-- | UITreatment
-- | ---------------------------------------------------------------------------
data UITreatment = UITreatment
  { _uiTreatmentSvid :: !Svid
  , _uiTreatmentTitle :: !Text
  , _uiTreatmentSku :: !Sku
  , _uiTreatmentProductId :: !Pid
  , _uiTreatmentPrice :: !Price
  , _uiTreatmentCurrency :: !Text
  , _uiTreatment :: !Int
  } deriving (Generic, Eq, Show)

makeLenses ''UITreatment

instance ToJSON UITreatment where
  toJSON treat = object
    [ "tag" .= ("UITreatment" :: Text)
    , "values" .= [
        object
          [ "_uiTreatmentSvid" .= _uiTreatmentSvid treat
          , "_uiTreatmentTitle" .= _uiTreatmentTitle treat
          , "_uiTreatmentSku" .= _uiTreatmentSku treat
          , "_uiTreatmentProductId" .= _uiTreatmentProductId treat
          , "_uiTreatmentPrice" .= _uiTreatmentPrice treat
          , "_uiTreatmentCurrency" .= _uiTreatmentCurrency treat
          , "_uiTreatment" .= _uiTreatment treat
          ]
      ]
    ]

instance FromJSON UITreatment

-- | ---------------------------------------------------------------------------
-- | UICampaign
-- | ---------------------------------------------------------------------------
data UICampaign = UICampaign
  { _uiCampaignId :: !CampaignId
  , _uiCampaignStart :: !(Maybe LocalTime)
  , _uiCampaignEnd :: !(Maybe LocalTime)
  , _uiCampaignTreatments :: ![UITreatment]
  } deriving (Generic, Eq, Show)

makeLenses ''UICampaign

instance ToJSON UICampaign where
  toJSON uiCmp = object
    [ "tag" .=  ("UICampaign" :: Text)
    , "values" .= [
        object
          [ "_uiCampaignId" .= _uiCampaignId uiCmp
          , "_uiCampaignStart" .= _uiCampaignStart uiCmp
          , "_uiCampaignEnd" .= _uiCampaignEnd uiCmp
          , "_uiCampaignTreatments" .= _uiCampaignTreatments uiCmp
          ]
      ]
    ]



instance FromJSON UICampaign

-- | ---------------------------------------------------------------------------
-- | VariantStats
-- | ---------------------------------------------------------------------------
data VariantStats = VariantStats
  { _varStatsSvid :: !Svid
  , _varStatsTreatment :: !Int
  , _varStatsUserCount :: !Int
  , _varStatsPrice :: !Price
  -- , _bsUserRevenues :: ![(UserId, Double)]
  } deriving (Eq, Generic, Show)

makeLenses ''VariantStats

instance ToJSON VariantStats

instance FromJSON VariantStats

-- | ---------------------------------------------------------------------------
-- | ExperimentStats
-- | ---------------------------------------------------------------------------
data ExperimentStats = ExperimentStats
  { _expStatsSku :: !Sku
  , _expStatsUserCount :: !Int
  , _expStatsVariants :: ![VariantStats]
  } deriving (Eq, Generic, Show)

makeLenses ''ExperimentStats

instance ToJSON ExperimentStats

instance FromJSON ExperimentStats

-- | ---------------------------------------------------------------------------
-- | CampaignStats
-- | ---------------------------------------------------------------------------
data CampaignStats = CampaignStats
  { _cmpStatsCampaignId :: !CampaignId
  , _cmpStatsCampaignName :: !Text
  , _cmpStatsStartDate :: !(Maybe LocalTime)
  , _cmpStatsEndDate :: !(Maybe LocalTime)
  , _cmpStatsExperiments :: ![ExperimentStats]
  -- , _cmpStatsProfitPerUserControl :: !(Estimate ConfInt Double)
  -- , _cmpStatsProfitPerUserTest :: !(Estimate ConfInt Double)
  -- , _cmpStatsConvertersControl :: ![Double]
  -- , _cmpStatsNonConvertersControl :: !Int
  -- , _cmpStatsConvertersTest :: ![Double]
  -- , _cmpStatsNonConvertersTest :: !Int
  -- , _cmpStatsConvertersControlCount :: !Int
  -- , _cmpStatsConvertersTestCount :: !Int
  -- , _cmpStatsConversionRateControl :: !Double
  -- , _cmpStatsConversionRateTest :: !Double
  } deriving (Eq, Generic, Show)

makeLenses ''CampaignStats

instance ToJSON CampaignStats

instance FromJSON CampaignStats

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
