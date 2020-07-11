module SweetSpot.Data.Codec where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (Json, decodeJson)
import Data.Argonaut.Decode ((.:))
import Data.DateTime (DateTime)
import Data.Either (Either(..), note)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Unsafe (unsafePerformEffect)
import SweetSpot.Data.Api (Image(..), InfResult(..), Product(..), UICampaign(..), UITreatment(..), UITreatmentVariant(..), Variant(..))

decodeInfResult :: Json -> Either String (Maybe InfResult)
decodeInfResult json =
  (
    do
      o <- decodeJson json
      lb <- o .: "_lowerBound"
      ub <- o .: "_upperBound"
      mean <- o .: "_mean"
      pure $ Just $
        InfResult
            { _lowerBound: lb
            , _upperBound: ub
            , _mean: mean
            }
  ) <|> pure Nothing

decodeUITreatmentVariant :: Json -> Either String UITreatmentVariant
decodeUITreatmentVariant json = do
  o <- decodeJson json
  title <- o .: "_uiTreatmentVariantTitle"
  sku <- o .: "_uiTreatmentSku"
  price <- o .: "_uiTreatmentVariantPrice"
  pure
    $ UITreatmentVariant
        { _uiTreatmentVariantTitle: title
        , _uiTreatmentSku: sku
        , _uiTreatmentVariantPrice: price
        }

decodeUITreatment :: Json -> Either String UITreatment
decodeUITreatment json = do
  o <- decodeJson json
  cr <- o .: "_uiTreatmentCR"
  aov <- o .: "_uiTreatmentAOV"
  vs <- o .: "_uiTreatmentVariants" >>= traverse decodeUITreatmentVariant
  pure
    $ UITreatment
        { _uiTreatmentCR: cr
        , _uiTreatmentAOV: aov
        , _uiTreatmentVariants: vs
        }

-- Brittle parser of ISO8601 / RFC3339
parseDateTime :: Maybe String -> Either String (Maybe DateTime)
parseDateTime Nothing = Right Nothing

parseDateTime (Just encodedDateTime) =
  JSDate.parse encodedDateTime
    # unsafePerformEffect
    >>> JSDate.toDateTime
    >>> note "failed to parse date"
    <#> Just

decodeUICampaign :: Json -> Either String UICampaign
decodeUICampaign json = do
  obj <- decodeJson json
  _uiCampaignId <- obj .: "_uiCampaignId"
  _uiCampaignName <- obj .: "_uiCampaignName"
  _uiCampaignStart <- obj .: "_uiCampaignStart" >>= parseDateTime
  _uiCampaignEnd <- obj .: "_uiCampaignEnd" >>= parseDateTime
  _uiCampaignLift <- obj .: "_uiCampaignLift" >>= decodeInfResult
  _uiCampaignAOVChange <- obj .: "_uiCampaignAOVChange"
  _uiCampaignCRChange <- obj .: "_uiCampaignCRChange"
  _uiCampaignCtrlTreatment <- obj .: "_uiCampaignCtrlTreatment" >>= decodeUITreatment
  _uiCampaignTestTreatment <- obj .: "_uiCampaignTestTreatment" >>= decodeUITreatment
  pure
    $ UICampaign
        { _uiCampaignId
        , _uiCampaignName
        , _uiCampaignStart
        , _uiCampaignEnd
        , _uiCampaignLift
        , _uiCampaignAOVChange
        , _uiCampaignCRChange
        , _uiCampaignCtrlTreatment
        , _uiCampaignTestTreatment
        }

decodeUICampaigns :: Json -> Either String (Array UICampaign)
decodeUICampaigns json = decodeJson json >>= traverse decodeUICampaign

decodeImage :: Json -> Either String Image
decodeImage json = do
  o <- decodeJson json
  construct <$> o .: "_imageSrc"
  where
  construct src = Image { _imageSrc: src }

decodeVariant :: Json -> Either String Variant
decodeVariant json = do
  o <- decodeJson json
  vid <- o .: "_variantId"
  pid <- o .: "_variantProductId"
  title <- o .: "_variantTitle"
  pTitle <- o .: "_variantProductTitle"
  sku <- o .: "_variantSku"
  price <- o .: "_variantPrice"
  pure
    $ Variant
        { _variantId: vid
        , _variantProductId: pid
        , _variantTitle: title
        , _variantProductTitle: pTitle
        , _variantSku: sku
        , _variantPrice: price
        }

decodeProduct :: Json -> Either String Product
decodeProduct json = do
  o <- decodeJson json
  pid <- o .: "_productId"
  title <- o .: "_productTitle"
  variants <- o .: "_productVariants" >>= traverse decodeVariant
  image <- o .: "_productImage" >>= decodeImage
  pure
    $ Product
        { _productId: pid
        , _productTitle: title
        , _productVariants: variants
        , _productImage: image
        }

decodeProducts :: Json -> Either String (Array Product)
decodeProducts json = do
  arr <- decodeJson json
  traverse decodeProduct arr
