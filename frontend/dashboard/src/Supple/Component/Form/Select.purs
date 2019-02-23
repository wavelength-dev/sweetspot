module Supple.Component.Form.Select where

import Prelude

import Data.Maybe (Maybe)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Supple.Component.Util (css)

type Option =
  { value :: String
  , label :: String }

type Input p =
  { label :: String
  , value :: String
  , options :: Array Option
  , onUpdate :: (String -> Maybe p)
  }

renderOption :: forall i p . Option -> HH.HTML i p
renderOption option =
  HH.option
    [ HP.value option.value ]
    [ HH.text option.label ]

component :: forall i p. Input p -> HH.HTML i p
component {label, value, options, onUpdate } =
  HH.div_
    [ HH.div
      [ css "Polaris-Labelled__LabelWrapper"]
      [ HH.div
        [ css "Polaris-Label" ]
        [ HH.label
          [ css "Polaris-Label__Text"
          , HP.id_ "Select1Label"
          , HP.for "Select1" ]
          [ HH.text label ]]]

    , HH.div_
      [ HH.select
        [ HP.value value
        , HE.onValueChange onUpdate ]
        ( renderOption <$> options )]
    ]
