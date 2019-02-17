module Supple.Component.Select where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Supple.Component.Util (css)
import Svg.Parser.Halogen (icon)

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

svg :: String
svg = """<svg class="Polaris-Icon__Svg" viewBox="0 0 20 20" focusable="false" aria-hidden="true"> <path d="M13 8l-3-3-3 3h6zm-.1 4L10 14.9 7.1 12h5.8z" fill-rule="evenodd"></path></svg>"""

type Icon = forall p r i. Array (HP.IProp r i) -> HH.HTML p i

selectIcon :: Icon
selectIcon = icon svg

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

      , HH.div
          [ css "Polaris-Select" ]
          [ HH.select
            [ css "Polaris-Select__Input"
            , HP.id_ "Select1"
            , HE.onValueChange $ HE.input UpdateValue ]
            [ HH.div_ $ renderOption <$> options ]]

      , HH.div
          [ css "Polaris-Select__Content" ]
          [ HH.span
            [ css "Polaris-Select__SelectedOption" ]
            [ HH.text "Hey" ]

          , HH.span
            [ css "Polaris-Select__Icon" ]
            [ HH.span
              [ css "Polaris-Icon" ]
              [ selectIcon [] ]]
          , HH.div
            [ css "Polaris-Select__Backdrop" ]
            []]]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    UpdateValue newValue a -> do
      { value } <- H.get
      when (newValue /= value) $ H.modify_ _ { value = newValue}
      H.raise $ Value newValue
      pure a
