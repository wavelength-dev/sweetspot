{-# LANGUAGE TemplateHaskell #-}

module SweetSpot.Data.Api where

import Control.Lens.TH (makeLenses)
import Data.Aeson
  ( (.:),
    (.=),
    FromJSON (..),
    ToJSON (..),
    object,
    withObject,
  )
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import RIO
import Servant (FromHttpApiData, ToHttpApiData)
import SweetSpot.Data.Common
import SweetSpot.Shopify.Types (FromShopJSON (..), Order (..))

-- | ---------------------------------------------------------------------------
-- | Image
-- | ---------------------------------------------------------------------------
data Image
  = Image
      { _imageSrc :: !Text
      }
  deriving (Eq, Generic, Show)

makeLenses ''Image

instance FromJSON Image

instance ToJSON Image

instance FromShopJSON Image where
  parseShopJSON = withObject "Image" $ \v ->
    Image
      <$> v .: "src"

-- | ---------------------------------------------------------------------------
-- | Variant
-- | ---------------------------------------------------------------------------
data Variant
  = Variant
      { _variantId :: !Svid,
        _variantProductId :: !Pid,
        _variantTitle :: !Text,
        _variantProductTitle :: !Text,
        _variantSku :: !Sku,
        _variantPrice :: !FormattedPrice
      }
  deriving (Eq, Generic, Show)

makeLenses ''Variant

instance ToJSON Variant

instance FromJSON Variant

-- | ---------------------------------------------------------------------------
-- | Product
-- | ---------------------------------------------------------------------------
data Product
  = Product
      { _productId :: !Pid,
        _productTitle :: !Text,
        _productVariants :: ![Variant],
        _productImage :: !(Maybe Image)
      }
  deriving (Eq, Generic, Show)

makeLenses ''Product

instance ToJSON Product

instance FromJSON Product

-- | ---------------------------------------------------------------------------
-- | Pagination
-- | ---------------------------------------------------------------------------
newtype PageInfo = PageInfo Text
  deriving (Eq, Generic, Show, FromHttpApiData, ToHttpApiData)

instance ToJSON PageInfo

data Pagination
  = Pagination
      { _paginationPrevious :: !(Maybe PageInfo),
        _paginationNext :: !(Maybe PageInfo)
      }
  deriving (Eq, Generic, Show)

makeLenses ''Pagination

instance ToJSON Pagination

-- | ---------------------------------------------------------------------------
-- | ProductsResponse
-- | ---------------------------------------------------------------------------
data ProductsResponse
  = ProductsResponse
      { _pagination :: !Pagination,
        _products :: ![Product]
      }
  deriving (Eq, Generic, Show)

instance ToJSON ProductsResponse

-- | ---------------------------------------------------------------------------
-- | InfResult
-- | ---------------------------------------------------------------------------
data InfResult
  = InfResult
      { _lowerBound :: !Double,
        _upperBound :: !Double,
        _mean :: !Double
      }
  deriving (Eq, Show, Generic)

instance ToJSON InfResult

instance FromJSON InfResult

-- | ---------------------------------------------------------------------------
-- | UITreatmentVariant
-- | ---------------------------------------------------------------------------
data UITreatmentVariant
  = UITreatmentVariant
      { _uiTreatmentVariantTitle :: !Text,
        _uiTreatmentSku :: !Sku,
        _uiTreatmentVariantPrice :: !FormattedPrice
      }
  deriving (Generic, Eq, Show)

makeLenses ''UITreatmentVariant

instance ToJSON UITreatmentVariant

instance FromJSON UITreatmentVariant

-- | ---------------------------------------------------------------------------
-- | UITreatment
-- | ---------------------------------------------------------------------------
data UITreatment
  = UITreatment
      { _uiTreatmentCR :: !(Maybe Double),
        _uiTreatmentAOV :: !FormattedPrice,
        _uiTreatmentVariants :: [UITreatmentVariant]
      }
  deriving (Generic, Eq, Show)

makeLenses ''UITreatment

instance ToJSON UITreatment

instance FromJSON UITreatment

-- | ---------------------------------------------------------------------------
-- | UICampaign
-- | ---------------------------------------------------------------------------
data UICampaign
  = UICampaign
      { _uiCampaignId :: !CampaignId,
        _uiCampaignName :: !Text,
        _uiCampaignStart :: !(Maybe UTCTime),
        _uiCampaignEnd :: !(Maybe UTCTime),
        _uiCampaignLift :: !(Maybe InfResult),
        _uiCampaignAOVChange :: !(Maybe Double),
        _uiCampaignCRChange :: !(Maybe Double),
        _uiCampaignCtrlTreatment :: !UITreatment,
        _uiCampaignTestTreatment :: !UITreatment,
        _uiCampaignUpdatedAt :: !UTCTime
      }
  deriving (Generic, Eq, Show)

makeLenses ''UICampaign

instance ToJSON UICampaign

instance FromJSON UICampaign

-- | ---------------------------------------------------------------------------
-- | CreateVariant
-- | ---------------------------------------------------------------------------
data CreateVariant
  = CreateVariant
      { _createVariantSvid :: !Svid,
        _createVariantPrice :: !Price
      }
  deriving (Eq, Generic, Show)

makeLenses ''CreateVariant

instance ToJSON CreateVariant

instance FromJSON CreateVariant

-- | ---------------------------------------------------------------------------
-- | CreateExperiment
-- | ---------------------------------------------------------------------------
data CreateExperiment
  = CreateExperiment
      { _createExperimentProductId :: !Pid,
        _createExperimentVariants :: ![CreateVariant]
      }
  deriving (Eq, Generic, Show)

makeLenses ''CreateExperiment

instance ToJSON CreateExperiment

instance FromJSON CreateExperiment

-- | ---------------------------------------------------------------------------
-- | CreateCampaign
-- | ---------------------------------------------------------------------------
data CreateCampaign
  = CreateCampaign
      { _createCampaignName :: !Text,
        _createCampaignEnd :: !(Maybe UTCTime),
        _createCampaignExperiments :: ![CreateExperiment]
      }
  deriving (Eq, Generic)

makeLenses ''CreateCampaign

instance FromJSON CreateCampaign

instance ToJSON CreateCampaign

-- | ---------------------------------------------------------------------------
-- | OkResponse
-- | ---------------------------------------------------------------------------
data OkResponse
  = OkResponse
      { message :: !Text
      }
  deriving (Eq, Generic, Show)

instance ToJSON OkResponse

instance FromJSON OkResponse

-- | ---------------------------------------------------------------------------
-- | TestMap
-- | ---------------------------------------------------------------------------
data TestMap
  = TestMap
      { _testMapUserId :: !UserId,
        _testMapTargetId :: !Svid,
        _testMapSku :: !Sku,
        _testMapSwapId :: !Svid,
        _testMapSwapPrice :: !FormattedPrice
      }
  deriving (Eq, Generic, Show)

instance ToJSON TestMap where
  toJSON (TestMap (UserId uid) (Svid targetId) sku (Svid swapId) price) =
    object
      [ "userId" .= uid,
        "variantId" .= targetId,
        "sku" .= sku,
        "swapId" .= swapId,
        "swapPrice" .= price
      ]

instance FromJSON TestMap where
  parseJSON = withObject "TestMap" $ \v -> do
    userId <- v .: "userId"
    targetId <- v .: "variantId"
    sku <- v .: "sku"
    swapId <- v .: "swapId"
    swapPrice <- v .: "swapPrice"
    return
      TestMap
        { _testMapUserId = UserId userId,
          _testMapTargetId = Svid targetId,
          _testMapSku = Sku sku,
          _testMapSwapId = Svid swapId,
          _testMapSwapPrice = FormattedPrice swapPrice
        }

-- | ---------------------------------------------------------------------------
-- | CartTokenReq
-- | ---------------------------------------------------------------------------
data CartTokenReq
  = CartTokenReq
      { _cartTokenReqToken :: CartToken,
        _cartTokenReqUser :: UserId
      }
  deriving (Generic, Eq, Show)

makeLenses ''CartTokenReq

instance FromJSON CartTokenReq

instance ToJSON CartTokenReq

-- | ---------------------------------------------------------------------------
-- | CheckoutPayload
-- | ---------------------------------------------------------------------------
data CheckoutPayload
  = CheckoutPayload
      { _checkoutPayloadUserId :: !UserId,
        _checkoutPayloadOrder :: !Order
      }
  deriving (Generic, Show)

makeLenses ''CheckoutPayload

instance FromJSON CheckoutPayload where
  parseJSON = withObject "CheckoutPayload" $ \v ->
    CheckoutPayload
      <$> v .: "user_id"
      <*> (v .: "order" >>= parseJSON)

instance ToJSON CheckoutPayload where
  toJSON (CheckoutPayload (UserId uid) order) =
    object
      [ "userId" .= uid,
        "order" .= (toJSON order)
      ]

-- | ---------------------------------------------------------------------------
-- | AppChargeStatusResponse
-- | ---------------------------------------------------------------------------
data AppChargeStatusResponse
  = AppChargeStatusResponse
      {_appChargeStatusResponse :: !AppChargeStatus}
  deriving (Generic)

instance ToJSON AppChargeStatusResponse
