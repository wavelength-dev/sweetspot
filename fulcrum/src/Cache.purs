module Fulcrum.Cache where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..))
import Data.Argonaut (decodeJson, encodeJson, fromString, jsonParser, printJsonDecodeError, stringify, toString) as Argonaut
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime) as D
import Data.Either (Either, note)
import Data.JSDate (fromDateTime, parse, toDateTime, toISOString) as JSDate
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Fulcrum.Data (TestMap)
import Web.HTML (window) as HTML
import Web.HTML.Window (localStorage) as Window
import Web.Storage.Storage (getItem, setItem) as Storage

newtype DateTime
  = DateTime D.DateTime

type CachedMaps
  = { created :: DateTime
    , testMaps :: Array TestMap
    }

testMapCacheKey :: String
testMapCacheKey = "sweetspot__test_maps"

instance decodeDateTime :: DecodeJson DateTime where
  decodeJson json = do
    str <- Argonaut.toString json # note (TypeMismatch "expected string")
    let
      jsDate = JSDate.parse str # unsafePerformEffect
    JSDate.toDateTime jsDate # note (UnexpectedValue json) <#> DateTime

instance encodeDateTime :: EncodeJson DateTime where
  encodeJson (DateTime dateTime) =
    JSDate.fromDateTime dateTime
      # JSDate.toISOString
      >>> unsafePerformEffect
      >>> Argonaut.fromString

decodeCachedTestMaps :: Json -> Either String CachedMaps
decodeCachedTestMaps = Argonaut.decodeJson >>> lmap Argonaut.printJsonDecodeError

getCachedTestMaps :: Effect (Either String CachedMaps)
getCachedTestMaps = do
  localStorage <- HTML.window >>= Window.localStorage
  eRawMaps <- Storage.getItem testMapCacheKey localStorage <#> note "no cached map available"
  eRawMaps >>= Argonaut.jsonParser >>= decodeCachedTestMaps # pure

setCachedTestMaps :: CachedMaps -> Effect Unit
setCachedTestMaps cachedMaps = do
  localStorage <- HTML.window >>= Window.localStorage
  let
    str = Argonaut.encodeJson cachedMaps # Argonaut.stringify
  Storage.setItem testMapCacheKey str localStorage
