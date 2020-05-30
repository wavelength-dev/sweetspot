module SweetSpot.Mock where

import Data.DateTime (DateTime)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
import Data.Maybe (fromJust) as Maybe
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import Prelude (pure, ($), (>>=), (>>>))
import SweetSpot.Data.Api (InfResult(..), UICampaign(..), UITreatment(..), UITreatmentVariant(..))

unsafeToDateTime :: String -> DateTime
unsafeToDateTime str =
  unsafePerformEffect
    $ JSDate.parse str
    >>= JSDate.toDateTime
    >>> unsafePartial Maybe.fromJust
    >>> pure

storewide10Campaign :: UICampaign
storewide10Campaign =
  UICampaign
    { _uiCampaignId: "idstring"
    , _uiCampaignStart: Just $ unsafeToDateTime "2020-02-23T06:15:41Z"
    , _uiCampaignEnd: Just $ unsafeToDateTime "2020-06-04T06:15:41Z"
    , _uiCampaignName: "Store-wide +10%"
    , _uiCampaignLift: InfResult { _lowerBound: 0.2, _upperBound: 2.7, _mean: 1.4 }
    , _uiCampaignAOVChange: 1.2
    , _uiCampaignCtrlTreatment:
        UITreatment
          { _uiTreatmentCR: 6.4e-2
          , _uiTreatmentAOV: "$324"
          , _uiTreatmentVariants:
              [ UITreatmentVariant
                  { _uiTreatmentVariantTitle: "Cup"
                  , _uiTreatmentSku: "6412"
                  , _uiTreatmentVariantPrice: "$349.99"
                  }
              ]
          }
    , _uiCampaignTestTreatment:
        UITreatment
          { _uiTreatmentCR: 6.1e-2
          , _uiTreatmentAOV: "$348"
          , _uiTreatmentVariants:
              [ UITreatmentVariant
                  { _uiTreatmentVariantTitle: "Cup"
                  , _uiTreatmentSku: "6412"
                  , _uiTreatmentVariantPrice: "$379.99"
                  }
              ]
          }
    }

expensiveJacketsCheapMonkeysCampaign :: UICampaign
expensiveJacketsCheapMonkeysCampaign =
  UICampaign
    { _uiCampaignId: "jacket-monkey"
    , _uiCampaignStart: Just $ unsafeToDateTime "2020-02-23T06:15:41Z"
    , _uiCampaignEnd: Just $ unsafeToDateTime "2020-02-26T06:15:41Z"
    , _uiCampaignName: "Expensive Jackets, Cheap Monkeys"
    , _uiCampaignLift: InfResult { _lowerBound: 0.2, _upperBound: 2.7, _mean: 1.4 }
    , _uiCampaignAOVChange: 1.2
    , _uiCampaignCtrlTreatment:
        UITreatment
          { _uiTreatmentCR: 6.4e-2
          , _uiTreatmentAOV: "$324"
          , _uiTreatmentVariants:
              [ UITreatmentVariant
                  { _uiTreatmentVariantTitle: "Brown leather jacket"
                  , _uiTreatmentSku: "6412"
                  , _uiTreatmentVariantPrice: "$349.99"
                  }
              , UITreatmentVariant
                  { _uiTreatmentVariantTitle: "Bonobo"
                  , _uiTreatmentSku: "242"
                  , _uiTreatmentVariantPrice: "$49.99"
                  }
              ]
          }
    , _uiCampaignTestTreatment:
        UITreatment
          { _uiTreatmentCR: 6.1e-2
          , _uiTreatmentAOV: "$348.0"
          , _uiTreatmentVariants:
              [ UITreatmentVariant
                  { _uiTreatmentVariantTitle: "Brown leather jacket"
                  , _uiTreatmentSku: "6412"
                  , _uiTreatmentVariantPrice: "$379.99"
                  }
              , UITreatmentVariant
                  { _uiTreatmentVariantTitle: "Bonobo"
                  , _uiTreatmentSku: "242"
                  , _uiTreatmentVariantPrice: "$29.99"
                  }
              ]
          }
    }
