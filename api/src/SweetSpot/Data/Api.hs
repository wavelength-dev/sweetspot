{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

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
import SweetSpot.Data.Common
import SweetSpot.Shopify.Types (FromShopJSON (..))

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
        _productImage :: !Image
      }
  deriving (Eq, Generic, Show)

makeLenses ''Product

instance ToJSON Product

instance FromJSON Product

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
        _uiCampaignTestTreatment :: !UITreatment
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
      { userId :: !UserId,
        targetId :: !Svid,
        sku :: !Sku,
        swapId :: !Svid,
        swapPrice :: !FormattedPrice
      }
  deriving (Eq, Generic, Show)

instance ToJSON TestMap where
  toJSON (TestMap (UserId userId) (Svid targetId) sku (Svid swapId) price) =
    object
      [ "userId" .= userId,
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
        { userId = UserId userId,
          targetId = Svid targetId,
          sku = Sku sku,
          swapId = Svid swapId,
          swapPrice = FormattedPrice swapPrice
        }

-- | ---------------------------------------------------------------------------
-- | CartTokenReq
-- | ---------------------------------------------------------------------------
data CartTokenReq
  = CartTokenReq
      { _cartTokenReqToken :: CartToken,
        _cartTokenReqUser :: UserId
      }
  deriving (Generic, Eq)

makeLenses ''CartTokenReq

instance FromJSON CartTokenReq

instance ToJSON CartTokenReq
