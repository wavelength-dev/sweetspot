module Main where

import Prelude

import AppM (Env, LogLevel(..), runAppM)
import Component.Router as Router
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff $ do
  let
    env :: Env
    env = { logLevel: Dev }

    rootComponent :: H.Component HH.HTML Router.Query Unit Void Aff
    rootComponent = H.hoist (runAppM env) Router.component

  body <- HA.awaitBody
  runUI rootComponent unit body
