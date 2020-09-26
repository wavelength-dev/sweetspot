module Fulcrum.TestMap where

import Prelude

import Control.Monad.Except (ExceptT, except, lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..))
import Data.Argonaut (decodeJson, encodeJson, fromString, jsonParser, printJsonDecodeError, stringify, toString) as Argonaut
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime) as D
import Data.DateTime (diff) as DateTime
import Data.Either (Either(..), note)
import Data.JSDate (fromDateTime, parse, toDateTime, toISOString) as JSDate
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Newtype (unwrap, wrap) as Newtype
import Data.Time.Duration (Days(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff (error, runAff_, throwError) as Aff
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime) as Now
import Effect.Unsafe (unsafePerformEffect)
import Fulcrum.Data (TestMapByVariant, TestMap)
import Fulcrum.Data (hashMapFromTestMaps) as Data
import Fulcrum.Logger (LogLevel(..))
import Fulcrum.Logger (log) as Logger
import Fulcrum.Service (TestMapProvisions(..))
import Fulcrum.Service (fetchTestMaps) as Service
import Fulcrum.User (UserId)
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

-- uses test maps with < 1 day age when available
-- always updates the cache in the background
getTestMap :: UserId -> ExceptT String Aff TestMapByVariant
getTestMap userId = do
  now <- Now.nowDateTime # liftEffect
  mapsCache <- getCachedTestMaps # liftEffect >>= except
  case mapsCache of
    EmptyCache -> do
      testMaps <- Service.fetchTestMaps (OnlyUserId userId) # lift >>= except
      setCachedTestMaps { created: now, testMaps } # liftEffect
      Data.hashMapFromTestMaps testMaps # pure
    CachedMaps { created, testMaps } -> do
      let
        isFresh = DateTime.diff created now < (Days 1.0)
      if isFresh then do
        -- fetch latest maps in the background
        Aff.runAff_ logResult do
          eNewTestMaps <- Service.fetchTestMaps (OnlyUserId userId)
          case eNewTestMaps of
            Left msg -> Aff.throwError (Aff.error msg)
            Right newTestMaps -> setCachedTestMaps { created: now, testMaps: newTestMaps } # liftEffect
          # liftEffect
        Data.hashMapFromTestMaps testMaps # pure
      else do
        newTestMaps <- Service.fetchTestMaps (OnlyUserId userId) # lift >>= except
        setCachedTestMaps { created: now, testMaps: newTestMaps } # liftEffect
        Data.hashMapFromTestMaps testMaps # pure
  where
  logResult (Left err) = Logger.log Error (show err)

  logResult (Right _) = pure unit
