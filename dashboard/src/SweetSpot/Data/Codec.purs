module SweetSpot.Data.Codec where

import Prelude

import Data.Argonaut (Json, decodeJson, getField)
import Data.Argonaut.Decode ((.:))
import Data.DateTime (DateTime(..))
import Data.Either (Either(..))
import Data.Formatter.DateTime as Formatter
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
import Data.Maybe (fromJust) as Maybe
import Data.Profunctor (unwrapIso)
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import SweetSpot.Data.Api (Image(..), InfResult(..), Product(..), UICampaign(..), UITreatment(..), UITreatmentVariant(..), Variant(..))
import Unsafe.Coerce (unsafeCoerce)

decodeInfResult :: Json -> Either String InfResult
decodeInfResult json = do
  o <- decodeJson json
  construct
    <$> o .: "_lowerBound"
    <*> o .: "_upperBound"
    <*> o .: "_mean"

  where
    construct lb ub mean =
      InfResult
        { _lowerBound: lb
        , _upperBound: ub
        , _mean: mean
        }

decodeUITreatmentVariant :: Json -> Either String UITreatmentVariant
decodeUITreatmentVariant json = do
  o <- decodeJson json
  construct
    <$> o .: "_uiTreatmentVariantTitle"
    <*> o .: "_uiTreatmentSku"
    <*> o .: "_uiTreatmentVariantPrice"
    <*> o .: "_uiTreatmentVariantCurrency"

  where
    construct title sku price currency =
      UITreatmentVariant
        { _uiTreatmentVariantTitle: title
        , _uiTreatmentSku: sku
        , _uiTreatmentVariantPrice: price
        , _uiTreatmentVariantCurrency: currency
        }

decodeUITreatment :: Json -> Either String UITreatment
decodeUITreatment json = do
  o <- decodeJson json
  construct
    <$> o .: "_uiTreatmentCR"
    <*> o .: "_uiTreatmentAOV"
    <*> (o .: "_uiTreatmentVariants" >>= traverse decodeUITreatmentVariant)

  where
    construct cr aov vs =
      UITreatment
        { _uiTreatmentCR: cr
        , _uiTreatmentAOV: aov
        , _uiTreatmentVariants: vs
        }

-- Brittle parser of ISO8601 / RFC3339
parseDateTime :: Maybe String -> Either String (Maybe DateTime)
parseDateTime Nothing = Right Nothing
parseDateTime (Just encodedDateTime) =
  Formatter.unformatDateTime "YYYY-MM-DDTHH:mm:ssZ" encodedDateTime
  <#> Just

decodeUICampaign :: Json -> Either String UICampaign
decodeUICampaign json = do
  obj <- decodeJson json
  _uiCampaignId <- obj .: "_uiCampaignId"
  _uiCampaignName <- obj .: "_uiCampaignName"
  _uiCampaignStart <- obj .: "_uiCampaignStart" >>= parseDateTime
  _uiCampaignEnd <- obj .: "_uiCampaignEnd" >>= parseDateTime
  _uiCampaignLift <- obj .: "_uiCampaignLift" >>= decodeInfResult
  _uiCampaignCtrlTreatment <- obj .: "_uiCampaignCtrlTreatment" >>= decodeUITreatment
  _uiCampaignTestTreatment <- obj .: "_uiCampaignTestTreatment" >>= decodeUITreatment
  pure
    $ UICampaign
        { _uiCampaignId
        , _uiCampaignName
        , _uiCampaignStart
        , _uiCampaignEnd
        , _uiCampaignLift
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
  construct
    <$> o .: "_variantId"
    <*> o .: "_variantProductId"
    <*> o .: "_variantTitle"
    <*> o .: "_variantSku"
    <*> o .: "_variantPrice"

  where
    construct vid pid title sku price =
      Variant
        { _variantId: vid
        , _variantProductId: pid
        , _variantTitle: title
        , _variantSku: sku
        , _variantPrice: price
        }

decodeProduct :: Json -> Either String Product
decodeProduct json = do
  o <- decodeJson json
  construct
    <$> o .: "_productId"
    <*> o .: "_productTitle"
    <*> (o .: "_productVariants" >>= traverse decodeVariant)
    <*> (o .: "_productImage" >>= decodeImage)

  where
    construct pid title variants image =
      Product
        { _productId: pid
        , _productTitle: title
        , _productVariants: variants
        , _productImage: image
        }

decodeProducts :: Json -> Either String (Array Product)
decodeProducts json = do
  arr <- decodeJson json
  traverse decodeProduct arr
