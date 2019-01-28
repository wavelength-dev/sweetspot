module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import AppM (Env, LogLevel(..), runAppM)
import Component.Router as Router
import Data.Api (Experiment, decodeResponse)
import Data.Array as A
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
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
  -- r <- fetchExperiments
  -- case r of
  --   Left s -> log $ "Fail: " <> s
  --   Right arr -> (log <<< show <<< A.length) arr
  runUI rootComponent unit body



fetchExperiments :: Aff (Either String (Array Experiment))
fetchExperiments =  do
  res <- AX.get ResponseFormat.json "/api/experiments"
  let decoded = case res.body of
        Left _ -> Left "Err"
        Right json -> decodeResponse json

  pure decoded
