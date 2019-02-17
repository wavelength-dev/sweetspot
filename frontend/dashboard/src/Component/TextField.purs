module Supple.Component.TextField where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Supple.Component.Util (css)

type State =
  { value :: String
  , placeholder :: String }

type Input = String

data Query a = UpdateValue String a

data Message = Value String


component :: forall m . H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState: \placeholder -> { placeholder, value: "" }
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ComponentHTML Query
  render { value, placeholder } =
    HH.div
      [css "Polaris-TextField"]
      [ HH.input
        [ HP.type_ HP.InputText
        , css "Polaris-TextField__Input"
        , HP.placeholder placeholder
        , HP.autofocus true
        , HP.value value
        , HE.onValueChange $ HE.input UpdateValue ]
      , HH.div
        [css "Polaris-TextField__Backdrop"]
        [HH.text ""]]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    UpdateValue newValue a -> do
      { value } <- H.get
      when (newValue /= value) $ H.modify_ _ { value = newValue}
      H.raise $ Value newValue
      pure a
