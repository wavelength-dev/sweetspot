-- File auto generated by purescript-bridge! --
module SweetSpot.Data.Api where

import Data.DateTime (DateTime)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe, Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(SProxy))
import Prim (Array, Number, String)

import Prelude

newtype InfResult =
    InfResult {
      _lowerBound :: Number
    , _upperBound :: Number
    , _mean :: Number
    }

derive instance eqInfResult :: Eq InfResult
derive instance genericInfResult :: Generic InfResult _
derive instance newtypeInfResult :: Newtype InfResult _

--------------------------------------------------------------------------------
_InfResult :: Iso' InfResult { _lowerBound :: Number, _upperBound :: Number, _mean :: Number}
_InfResult = _Newtype

lowerBound :: Lens' InfResult Number
lowerBound = _Newtype <<< prop (SProxy :: SProxy "_lowerBound")

upperBound :: Lens' InfResult Number
upperBound = _Newtype <<< prop (SProxy :: SProxy "_upperBound")

mean :: Lens' InfResult Number
mean = _Newtype <<< prop (SProxy :: SProxy "_mean")

--------------------------------------------------------------------------------
newtype UICampaign =
    UICampaign {
      _uiCampaignId :: String
    , _uiCampaignName :: String
    , _uiCampaignStart :: Maybe DateTime
    , _uiCampaignEnd :: Maybe DateTime
    , _uiCampaignLift :: Maybe InfResult
    , _uiCampaignAOVChange :: Maybe Number
    , _uiCampaignCRChange :: Maybe Number
    , _uiCampaignCtrlTreatment :: UITreatment
    , _uiCampaignTestTreatment :: UITreatment
    }

derive instance eqUICampaign :: Eq UICampaign
derive instance genericUICampaign :: Generic UICampaign _
derive instance newtypeUICampaign :: Newtype UICampaign _

--------------------------------------------------------------------------------
_UICampaign :: Iso' UICampaign { _uiCampaignId :: String, _uiCampaignName :: String, _uiCampaignStart :: Maybe DateTime, _uiCampaignEnd :: Maybe DateTime, _uiCampaignLift :: Maybe InfResult, _uiCampaignAOVChange :: Maybe Number, _uiCampaignCRChange :: Maybe Number, _uiCampaignCtrlTreatment :: UITreatment, _uiCampaignTestTreatment :: UITreatment}
_UICampaign = _Newtype

uiCampaignId :: Lens' UICampaign String
uiCampaignId = _Newtype <<< prop (SProxy :: SProxy "_uiCampaignId")

uiCampaignName :: Lens' UICampaign String
uiCampaignName = _Newtype <<< prop (SProxy :: SProxy "_uiCampaignName")

uiCampaignStart :: Lens' UICampaign (Maybe DateTime)
uiCampaignStart = _Newtype <<< prop (SProxy :: SProxy "_uiCampaignStart")

uiCampaignEnd :: Lens' UICampaign (Maybe DateTime)
uiCampaignEnd = _Newtype <<< prop (SProxy :: SProxy "_uiCampaignEnd")

uiCampaignLift :: Lens' UICampaign (Maybe InfResult)
uiCampaignLift = _Newtype <<< prop (SProxy :: SProxy "_uiCampaignLift")

uiCampaignAOVChange :: Lens' UICampaign (Maybe Number)
uiCampaignAOVChange = _Newtype <<< prop (SProxy :: SProxy "_uiCampaignAOVChange")

uiCampaignCRChange :: Lens' UICampaign (Maybe Number)
uiCampaignCRChange = _Newtype <<< prop (SProxy :: SProxy "_uiCampaignCRChange")

uiCampaignCtrlTreatment :: Lens' UICampaign UITreatment
uiCampaignCtrlTreatment = _Newtype <<< prop (SProxy :: SProxy "_uiCampaignCtrlTreatment")

uiCampaignTestTreatment :: Lens' UICampaign UITreatment
uiCampaignTestTreatment = _Newtype <<< prop (SProxy :: SProxy "_uiCampaignTestTreatment")

--------------------------------------------------------------------------------
newtype UITreatment =
    UITreatment {
      _uiTreatmentCR :: Maybe Number
    , _uiTreatmentAOV :: String
    , _uiTreatmentVariants :: Array UITreatmentVariant
    }

derive instance eqUITreatment :: Eq UITreatment
derive instance genericUITreatment :: Generic UITreatment _
derive instance newtypeUITreatment :: Newtype UITreatment _

--------------------------------------------------------------------------------
_UITreatment :: Iso' UITreatment { _uiTreatmentCR :: Maybe Number, _uiTreatmentAOV :: String, _uiTreatmentVariants :: Array UITreatmentVariant}
_UITreatment = _Newtype

uiTreatmentCR :: Lens' UITreatment (Maybe Number)
uiTreatmentCR = _Newtype <<< prop (SProxy :: SProxy "_uiTreatmentCR")

uiTreatmentAOV :: Lens' UITreatment String
uiTreatmentAOV = _Newtype <<< prop (SProxy :: SProxy "_uiTreatmentAOV")

uiTreatmentVariants :: Lens' UITreatment (Array UITreatmentVariant)
uiTreatmentVariants = _Newtype <<< prop (SProxy :: SProxy "_uiTreatmentVariants")

--------------------------------------------------------------------------------
newtype UITreatmentVariant =
    UITreatmentVariant {
      _uiTreatmentVariantTitle :: String
    , _uiTreatmentSku :: String
    , _uiTreatmentVariantPrice :: String
    }

derive instance eqUITreatmentVariant :: Eq UITreatmentVariant
derive instance genericUITreatmentVariant :: Generic UITreatmentVariant _
derive instance newtypeUITreatmentVariant :: Newtype UITreatmentVariant _

--------------------------------------------------------------------------------
_UITreatmentVariant :: Iso' UITreatmentVariant { _uiTreatmentVariantTitle :: String, _uiTreatmentSku :: String, _uiTreatmentVariantPrice :: String}
_UITreatmentVariant = _Newtype

uiTreatmentVariantTitle :: Lens' UITreatmentVariant String
uiTreatmentVariantTitle = _Newtype <<< prop (SProxy :: SProxy "_uiTreatmentVariantTitle")

uiTreatmentSku :: Lens' UITreatmentVariant String
uiTreatmentSku = _Newtype <<< prop (SProxy :: SProxy "_uiTreatmentSku")

uiTreatmentVariantPrice :: Lens' UITreatmentVariant String
uiTreatmentVariantPrice = _Newtype <<< prop (SProxy :: SProxy "_uiTreatmentVariantPrice")

--------------------------------------------------------------------------------
newtype Image =
    Image {
      _imageSrc :: String
    }

derive instance eqImage :: Eq Image
derive instance genericImage :: Generic Image _
derive instance newtypeImage :: Newtype Image _

--------------------------------------------------------------------------------
_Image :: Iso' Image { _imageSrc :: String}
_Image = _Newtype

imageSrc :: Lens' Image String
imageSrc = _Newtype <<< prop (SProxy :: SProxy "_imageSrc")

--------------------------------------------------------------------------------
newtype Variant =
    Variant {
      _variantId :: String
    , _variantProductId :: String
    , _variantTitle :: String
    , _variantSku :: String
    , _variantPrice :: String
    }

derive instance eqVariant :: Eq Variant
derive instance genericVariant :: Generic Variant _
derive instance newtypeVariant :: Newtype Variant _

--------------------------------------------------------------------------------
_Variant :: Iso' Variant { _variantId :: String, _variantProductId :: String, _variantTitle :: String, _variantSku :: String, _variantPrice :: String}
_Variant = _Newtype

variantId :: Lens' Variant String
variantId = _Newtype <<< prop (SProxy :: SProxy "_variantId")

variantProductId :: Lens' Variant String
variantProductId = _Newtype <<< prop (SProxy :: SProxy "_variantProductId")

variantTitle :: Lens' Variant String
variantTitle = _Newtype <<< prop (SProxy :: SProxy "_variantTitle")

variantSku :: Lens' Variant String
variantSku = _Newtype <<< prop (SProxy :: SProxy "_variantSku")

variantPrice :: Lens' Variant String
variantPrice = _Newtype <<< prop (SProxy :: SProxy "_variantPrice")

--------------------------------------------------------------------------------
newtype Product =
    Product {
      _productId :: String
    , _productTitle :: String
    , _productVariants :: Array Variant
    , _productImage :: Image
    }

derive instance eqProduct :: Eq Product
derive instance genericProduct :: Generic Product _
derive instance newtypeProduct :: Newtype Product _

--------------------------------------------------------------------------------
_Product :: Iso' Product { _productId :: String, _productTitle :: String, _productVariants :: Array Variant, _productImage :: Image}
_Product = _Newtype

productId :: Lens' Product String
productId = _Newtype <<< prop (SProxy :: SProxy "_productId")

productTitle :: Lens' Product String
productTitle = _Newtype <<< prop (SProxy :: SProxy "_productTitle")

productVariants :: Lens' Product (Array Variant)
productVariants = _Newtype <<< prop (SProxy :: SProxy "_productVariants")

productImage :: Lens' Product Image
productImage = _Newtype <<< prop (SProxy :: SProxy "_productImage")

--------------------------------------------------------------------------------
newtype CartTokenReq =
    CartTokenReq {
      _cartTokenReqToken :: String
    , _cartTokenReqUser :: String
    }

derive instance eqCartTokenReq :: Eq CartTokenReq
derive instance genericCartTokenReq :: Generic CartTokenReq _
derive instance newtypeCartTokenReq :: Newtype CartTokenReq _

--------------------------------------------------------------------------------
_CartTokenReq :: Iso' CartTokenReq { _cartTokenReqToken :: String, _cartTokenReqUser :: String}
_CartTokenReq = _Newtype

cartTokenReqToken :: Lens' CartTokenReq String
cartTokenReqToken = _Newtype <<< prop (SProxy :: SProxy "_cartTokenReqToken")

cartTokenReqUser :: Lens' CartTokenReq String
cartTokenReqUser = _Newtype <<< prop (SProxy :: SProxy "_cartTokenReqUser")

--------------------------------------------------------------------------------
newtype CreateExperiment =
    CreateExperiment {
      _createExperimentProductId :: String
    , _createExperimentPrice :: Number
    }

derive instance eqCreateExperiment :: Eq CreateExperiment
derive instance genericCreateExperiment :: Generic CreateExperiment _
derive instance newtypeCreateExperiment :: Newtype CreateExperiment _

--------------------------------------------------------------------------------
_CreateExperiment :: Iso' CreateExperiment { _createExperimentProductId :: String, _createExperimentPrice :: Number}
_CreateExperiment = _Newtype

createExperimentProductId :: Lens' CreateExperiment String
createExperimentProductId = _Newtype <<< prop (SProxy :: SProxy "_createExperimentProductId")

createExperimentPrice :: Lens' CreateExperiment Number
createExperimentPrice = _Newtype <<< prop (SProxy :: SProxy "_createExperimentPrice")

--------------------------------------------------------------------------------
newtype CreateCampaign =
    CreateCampaign {
      _createCampaignName :: String
    , _createCampaignEnd :: Maybe DateTime
    , _createCampaignExperiments :: Array CreateExperiment
    }

derive instance eqCreateCampaign :: Eq CreateCampaign
derive instance genericCreateCampaign :: Generic CreateCampaign _
derive instance newtypeCreateCampaign :: Newtype CreateCampaign _

--------------------------------------------------------------------------------
_CreateCampaign :: Iso' CreateCampaign { _createCampaignName :: String, _createCampaignEnd :: Maybe DateTime, _createCampaignExperiments :: Array CreateExperiment}
_CreateCampaign = _Newtype

createCampaignName :: Lens' CreateCampaign String
createCampaignName = _Newtype <<< prop (SProxy :: SProxy "_createCampaignName")

createCampaignEnd :: Lens' CreateCampaign (Maybe DateTime)
createCampaignEnd = _Newtype <<< prop (SProxy :: SProxy "_createCampaignEnd")

createCampaignExperiments :: Lens' CreateCampaign (Array CreateExperiment)
createCampaignExperiments = _Newtype <<< prop (SProxy :: SProxy "_createCampaignExperiments")

--------------------------------------------------------------------------------
