module SweetSpot.Data.Codec where

import Prelude
import SweetSpot.Data.Api

import Data.Argonaut (decodeJson, Json)
import Data.Argonaut.Decode ((.:))
import Data.Either (Either)
import Data.Traversable (traverse)

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


decodeUICampaign :: Json -> Either String UICampaign
decodeUICampaign json = do
  o <- decodeJson json
  construct
    <$> o .: "_uiCampaignId"
    <*> o .: "_uiCampaignName"
    <*> o .: "_uiCampaignStart"
    <*> o .: "_uiCampaignEnd"
    <*> (o .: "_uiCampaignLift" >>= decodeInfResult)
    <*> (o .: "_uiCampaignCtrlTreatment" >>= decodeUITreatment)
    <*> (o .: "_uiCampaignTestTreatment" >>= decodeUITreatment)

  where
    construct id name start end lift ctrlTreatment testTreatment =
      UICampaign
        { _uiCampaignId: id
        , _uiCampaignName: name
        , _uiCampaignStart: start
        , _uiCampaignEnd: end
        , _uiCampaignLift: lift
        , _uiCampaignCtrlTreatment: ctrlTreatment
        , _uiCampaignTestTreatment: testTreatment
        }

decodeUICampaigns :: Json -> Either String (Array UICampaign)
decodeUICampaigns json = do
  arr <- decodeJson json
  traverse decodeUICampaign arr

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
