module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import App as App
import Data (Experiment, decodeResponse)
import Data.Array as A
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  r <- fetchExperiments
  case r of
    Left s -> log $ "Fail: " <> s
    Right arr -> (log <<< show <<< A.length) arr
  runUI App.component unit body


fetchExperiments :: Aff (Either String (Array Experiment))
fetchExperiments =  do
  res <- AX.get ResponseFormat.json "/api/experiments"
  let decoded = case res.body of
        Left _ -> Left "Err"
        Right json -> decodeResponse json

  pure decoded
