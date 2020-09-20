module Fulcrum.Cache where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut (decodeJson, encodeJson, fromString, jsonParser, stringify, toString) as Argonaut
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

testMapCacheKey :: String
testMapCacheKey = "sweetspot__test_maps"

instance decodeInstant :: DecodeJson DateTime where
  decodeJson json = do
    str <- Argonaut.toString json # note "expected string"
    let
      jsDate = JSDate.parse str # unsafePerformEffect
    JSDate.toDateTime jsDate # note "not a valid date" <#> DateTime

instance encodeDateTime :: EncodeJson DateTime where
  encodeJson (DateTime dateTime) =
    JSDate.fromDateTime dateTime
      # JSDate.toISOString
      >>> unsafePerformEffect
      >>> Argonaut.fromString

type CachedMaps
  = { created :: DateTime
    , testMaps :: Array TestMap
    }

decodeCachedTestMaps :: String -> Either String CachedMaps
decodeCachedTestMaps = Argonaut.jsonParser >=> Argonaut.decodeJson

getCachedTestMaps :: Effect (Either String CachedMaps)
getCachedTestMaps = do
  localStorage <- HTML.window >>= Window.localStorage
  eRawMaps <- Storage.getItem testMapCacheKey localStorage <#> note "no cached map available"
  eRawMaps >>= decodeCachedTestMaps # pure

setCachedTestMaps :: CachedMaps -> Effect Unit
setCachedTestMaps testMaps = do
  localStorage <- HTML.window >>= Window.localStorage
  let
    str = Argonaut.encodeJson testMaps # Argonaut.stringify
  Storage.setItem testMapCacheKey str localStorage
