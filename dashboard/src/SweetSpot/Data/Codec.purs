module SweetSpot.Data.Codec where

import Prelude
import SweetSpot.Data.Api (Image(..), InfResult(..), Product(..), UICampaign(..), UITreatment(..), UITreatmentVariant(..), Variant(..))
import Data.Argonaut (Json, decodeJson, getField)
import Data.Argonaut.Decode ((.:))
import Data.Either (Either(..))
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
import Data.Maybe (fromJust) as Maybe
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)

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


decodeUICampaign :: Json -> Effect (Either String UICampaign)
decodeUICampaign json = do
  e_uiCampaignStart <- case (decodeJson json >>= \obj -> getField obj "_uiCampaignStart") of
    Left err -> pure $ Left err
    Right (Nothing) -> pure $ Right Nothing
    Right (Just start) -> do
      startJSDate <- JSDate.parse start
      let
        startDateTime = JSDate.toDateTime startJSDate # (unsafePartial Maybe.fromJust)
      pure $ Right $ Just startDateTime
  e_uiCampaignEnd <- case (decodeJson json >>= \obj -> getField obj "_uiCampaignEnd") of
    Left err -> pure $ Left err
    Right (Nothing) -> pure $ Right Nothing
    Right (Just end) -> do
      endJSDate <- JSDate.parse end
      let
        endDateTime = JSDate.toDateTime endJSDate # (unsafePartial Maybe.fromJust)
      pure $ Right $ Just endDateTime
  pure
    $ do
        _uiCampaignStart <- e_uiCampaignStart
        _uiCampaignEnd <- e_uiCampaignEnd
        o <- decodeJson json
        _uiCampaignId <- o .: "_uiCampaignId"
        _uiCampaignName <- o .: "_uiCampaignName"
        _uiCampaignLift <- o .: "_uiCampaignLift" >>= decodeInfResult
        _uiCampaignCtrlTreatment <- o .: "_uiCampaignCtrlTreatment" >>= decodeUITreatment
        _uiCampaignTestTreatment <- o .: "_uiCampaignTestTreatment" >>= decodeUITreatment
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

decodeUICampaigns :: Json -> Effect (Either String (Array UICampaign))
decodeUICampaigns json = do
  let
    eArray = decodeJson json
  case eArray of
    Left errMsg -> pure $ Left errMsg
    Right array -> traverse decodeUICampaign array >>= sequence >>> pure

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
