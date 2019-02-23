module Supple.Component.Form.TextField where

import Data.Maybe (Maybe)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Supple.Component.Util (css)

type Input p =
  { placeholder :: String
  , value :: String
  , onUpdate :: (String -> Maybe p)
  }

component :: forall i p. Input p -> HH.HTML i p
component { placeholder, value, onUpdate } =
    HH.div
      [css "Polaris-TextField"]
      [ HH.input
        [ HP.type_ HP.InputText
        , css "Polaris-TextField__Input"
        , HP.placeholder placeholder
        , HP.autofocus true
        , HP.value value
        , HE.onValueInput onUpdate ]
      , HH.div
        [css "Polaris-TextField__Backdrop"]
        []]
