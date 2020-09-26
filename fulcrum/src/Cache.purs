module Fulcrum.Cache where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..))
import Data.Argonaut (decodeJson, encodeJson, fromString, jsonParser, printJsonDecodeError, stringify, toString) as Argonaut
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime) as D
import Data.Either (Either(..), note)
import Data.JSDate (fromDateTime, parse, toDateTime, toISOString) as JSDate
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Newtype (unwrap, wrap) as Newtype
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Fulcrum.Data (TestMap)
import Web.HTML (window) as HTML
import Web.HTML.Window (localStorage) as Window
import Web.Storage.Storage (getItem, setItem) as Storage

-- To be able to write Decode- and EncodeJson instances we wrap
-- DateTime in our own newtype.
newtype DateTime
  = DateTime D.DateTime

derive instance newtypeDateTime :: Newtype DateTime _

type CacheableMapsWrapped
  = { created :: DateTime
    , testMaps :: Array TestMap
    }

type CacheableMaps
  = { created :: D.DateTime
    , testMaps :: Array TestMap
    }

unwrapMaps :: CacheableMapsWrapped -> CacheableMaps
unwrapMaps { created, testMaps } = { created: Newtype.unwrap created, testMaps }

wrapMaps :: CacheableMaps -> CacheableMapsWrapped
wrapMaps { created, testMaps } = { created: Newtype.wrap created, testMaps }

data TestMapsCache
  = CachedMaps CacheableMaps
  | EmptyCache

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

decodeCachedTestMaps :: Json -> Either String CacheableMapsWrapped
decodeCachedTestMaps = Argonaut.decodeJson >>> lmap Argonaut.printJsonDecodeError

getCachedTestMaps :: Effect (Either String TestMapsCache)
getCachedTestMaps =
  HTML.window
    >>= Window.localStorage
    >>= Storage.getItem testMapCacheKey
    <#> case _ of
        Nothing -> Right EmptyCache
        Just str -> Argonaut.jsonParser str >>= decodeCachedTestMaps <#> unwrapMaps >>> CachedMaps

setCachedTestMaps :: CacheableMaps -> Effect Unit
setCachedTestMaps cacheableMaps = do
  localStorage <- HTML.window >>= Window.localStorage
  let
    cacheableStr = wrapMaps cacheableMaps # Argonaut.encodeJson >>> Argonaut.stringify
  Storage.setItem testMapCacheKey cacheableStr localStorage
