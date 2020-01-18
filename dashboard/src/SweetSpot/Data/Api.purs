-- File auto generated by purescript-bridge! --
module SweetSpot.Data.Api where

import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe, Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(SProxy))
import Prim (Array, Int, Number, String)

import Prelude

newtype UICampaign =
    UICampaign {
      _uiCampaignId :: String
    , _uiCampaignStart :: Maybe String
    , _uiCampaignEnd :: Maybe String
    , _uiCampaignTreatments :: Array UITreatment
    }

derive instance eqUICampaign :: Eq UICampaign
derive instance genericUICampaign :: Generic UICampaign _
derive instance newtypeUICampaign :: Newtype UICampaign _

--------------------------------------------------------------------------------
_UICampaign :: Iso' UICampaign { _uiCampaignId :: String, _uiCampaignStart :: Maybe String, _uiCampaignEnd :: Maybe String, _uiCampaignTreatments :: Array UITreatment}
_UICampaign = _Newtype

uiCampaignId :: Lens' UICampaign String
uiCampaignId = _Newtype <<< prop (SProxy :: SProxy "_uiCampaignId")

uiCampaignStart :: Lens' UICampaign (Maybe String)
uiCampaignStart = _Newtype <<< prop (SProxy :: SProxy "_uiCampaignStart")

uiCampaignEnd :: Lens' UICampaign (Maybe String)
uiCampaignEnd = _Newtype <<< prop (SProxy :: SProxy "_uiCampaignEnd")

uiCampaignTreatments :: Lens' UICampaign (Array UITreatment)
uiCampaignTreatments = _Newtype <<< prop (SProxy :: SProxy "_uiCampaignTreatments")

--------------------------------------------------------------------------------
newtype UITreatment =
    UITreatment {
      _uiTreatmentSvid :: String
    , _uiTreatmentTitle :: String
    , _uiTreatmentSku :: String
    , _uiTreatmentProductId :: String
    , _uiTreatmentPrice :: Number
    , _uiTreatmentCurrency :: String
    , _uiTreatment :: Int
    }

derive instance eqUITreatment :: Eq UITreatment
derive instance genericUITreatment :: Generic UITreatment _
derive instance newtypeUITreatment :: Newtype UITreatment _

--------------------------------------------------------------------------------
_UITreatment :: Iso' UITreatment { _uiTreatmentSvid :: String, _uiTreatmentTitle :: String, _uiTreatmentSku :: String, _uiTreatmentProductId :: String, _uiTreatmentPrice :: Number, _uiTreatmentCurrency :: String, _uiTreatment :: Int}
_UITreatment = _Newtype

uiTreatmentSvid :: Lens' UITreatment String
uiTreatmentSvid = _Newtype <<< prop (SProxy :: SProxy "_uiTreatmentSvid")

uiTreatmentTitle :: Lens' UITreatment String
uiTreatmentTitle = _Newtype <<< prop (SProxy :: SProxy "_uiTreatmentTitle")

uiTreatmentSku :: Lens' UITreatment String
uiTreatmentSku = _Newtype <<< prop (SProxy :: SProxy "_uiTreatmentSku")

uiTreatmentProductId :: Lens' UITreatment String
uiTreatmentProductId = _Newtype <<< prop (SProxy :: SProxy "_uiTreatmentProductId")

uiTreatmentPrice :: Lens' UITreatment Number
uiTreatmentPrice = _Newtype <<< prop (SProxy :: SProxy "_uiTreatmentPrice")

uiTreatmentCurrency :: Lens' UITreatment String
uiTreatmentCurrency = _Newtype <<< prop (SProxy :: SProxy "_uiTreatmentCurrency")

uiTreatment :: Lens' UITreatment Int
uiTreatment = _Newtype <<< prop (SProxy :: SProxy "_uiTreatment")

--------------------------------------------------------------------------------
