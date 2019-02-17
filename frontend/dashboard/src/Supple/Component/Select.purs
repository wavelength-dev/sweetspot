module Supple.Component.Select where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Supple.Component.Util (css)

type Option =
  { value :: String
  , label :: String }

type State =
  { value :: String
  , label :: String
  , options :: Array Option }

type Input =
  { label :: String
  , options :: Array Option}

data Query a = UpdateValue String a

data Message = Value String

component :: forall m . H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState: \{ label, options } -> { label, options, value: "" }
    , render
    , eval
    , receiver: const Nothing
    }
  where

  renderOption :: forall i p . Option -> HH.HTML i p
  renderOption option =
    HH.option
     [ HP.value option.value ]
     [ HH.text option.label ]

  render :: State -> H.ComponentHTML Query
  render { value, label, options } =
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
            [ HE.onValueChange $ HE.input UpdateValue ]
            ( renderOption <$> options )]]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    UpdateValue newValue a -> do
      { value } <- H.get
      when (newValue /= value) $ H.modify_ _ { value = newValue}
      H.raise $ Value newValue
      pure a
