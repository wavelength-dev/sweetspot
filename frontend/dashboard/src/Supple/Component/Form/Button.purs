module Supple.Component.Form.Button where


import Halogen.HTML as HH
import Data.Maybe (Maybe)
import Halogen.HTML.Events as HE
import Supple.Component.Util (css)
import Web.UIEvent.MouseEvent (MouseEvent)

component :: forall i p . String -> (MouseEvent -> Maybe p) -> HH.HTML i p
component label onClick =
  HH.button
  [ css "Polaris-Button"
  , HE.onClick onClick ]
  [ HH.span
    [ css "Polaris-Button__Content"]
    [ HH.span
      [ css "Polaris-Button__Text" ]
      [ HH.text label ]]]
