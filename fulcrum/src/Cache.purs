module Fulcrum.Cache where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..))
import Data.Argonaut (decodeJson, encodeJson, fromNumber, jsonParser, printJsonDecodeError, stringify, toNumber) as Argonaut
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime) as D
import Data.DateTime.Instant as Instant
import Data.Either (Either, note)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Newtype (unwrap) as Newtype
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Fulcrum.Data (TestMap)
import Web.HTML (window) as HTML
import Web.HTML.Window (localStorage) as Window
import Web.Storage.Storage (getItem, setItem) as Storage

newtype DateTime
  = DateTime D.DateTime

derive instance newtypeDateTime :: Newtype DateTime _

testMapCacheKey :: String
testMapCacheKey = "sweetspot__test_maps"

instance decodeJsonDateTime :: DecodeJson DateTime where
  decodeJson json = (toNumber json <#> Milliseconds) >>= toInstant <#> Instant.toDateTime >>> DateTime :: Either JsonDecodeError DateTime
    where
    toNumber = Argonaut.toNumber >>> (note (TypeMismatch "timestamp not a number") :: forall a. Maybe a -> Either JsonDecodeError a)

    toInstant = Instant.instant >>> note (TypeMismatch "valid timestamp but invalid moment in time")

instance encodeJsonDateTime :: EncodeJson DateTime where
  encodeJson =
    Newtype.unwrap >>> Instant.fromDateTime
      >>> Instant.unInstant
      >>> Newtype.unwrap
      >>> Argonaut.fromNumber

type CachedMaps
  = { created :: D.DateTime, testMaps :: Array TestMap }

type CachedMaps'
  = { created :: DateTime, testMaps :: Array TestMap }

decodeCachedTestMaps :: Json -> Either JsonDecodeError CachedMaps
decodeCachedTestMaps = decodeJson >>> map \{ created, testMaps } -> { created: Newtype.unwrap created, testMaps }
  where
  decodeJson :: Json -> Either JsonDecodeError CachedMaps'
  decodeJson = Argonaut.decodeJson

getCachedTestMaps :: Effect (Either String CachedMaps)
getCachedTestMaps = do
  localStorage <- HTML.window >>= Window.localStorage
  eRawMaps <- Storage.getItem testMapCacheKey localStorage <#> note "no cached map available"
  eRawMaps
    >>= ( Argonaut.jsonParser
          >=> decodeCachedTestMaps
          >>> lmap Argonaut.printJsonDecodeError
      )
    # pure

setCachedTestMaps :: CachedMaps -> Effect Unit
setCachedTestMaps { created, testMaps } = do
  localStorage <- HTML.window >>= Window.localStorage
  let
    jsonStr = Argonaut.encodeJson { created: DateTime created, testMaps } # Argonaut.stringify
  Storage.setItem testMapCacheKey jsonStr localStorage
