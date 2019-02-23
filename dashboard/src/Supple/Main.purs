module Main where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.Hash (getHash, matchesWith)
import Supple.AppM (Env, LogLevel(..), runAppM)
import Supple.Component.Router as Router
import Supple.Data.Route (Route, routeCodec)

main :: Effect Unit
main = HA.runHalogenAff $ do
  initialHash <- liftEffect $ getHash
  let
    env :: Env
    env = { logLevel: Dev }

    rootComponent :: H.Component HH.HTML Router.Query (Maybe Route) Void Aff
    rootComponent = H.hoist (runAppM env) Router.component

    initialRoute :: Maybe Route
    initialRoute = hush $ parse routeCodec initialHash


  body <- HA.awaitBody
  halogenIO <- runUI rootComponent initialRoute body

  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      launchAff_ $ halogenIO.query $ H.action $ Router.Navigate new

  pure unit
