module Main where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Argonaut (Json, decodeJson, (.:))
import Data.Argonaut as Argonaut
import Data.DateTime (diff) as DateTime
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map (empty, fromFoldable) as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (class Duration, Milliseconds(..), Minutes(..))
import Data.Time.Duration (convertDuration) as Duration
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff (makeAff, nonCanceler, runAff_) as Aff
import Effect.Class (liftEffect)
import Effect.Console (logShow) as Console
import Effect.Exception (error)
import Effect.Now as Now
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Fulcrum.Data (TestMap, VariantId(..))
import Fulcrum.Logging (LogLevel(..)) as LogLevel
import Fulcrum.Logging (log) as Logging
import Fulcrum.MemoryStore (get) as MemoryStore
import Fulcrum.RuntimeDependency (getIsRuntimeAdequate) as RuntimeDependency
import Fulcrum.User (getUserId) as User
import Web.HTML (window) as HTML
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage

type TestContext
  = { variantTestMap :: Map VariantId TestMap }

-- or pass a function
decodeCachedTestMaps :: Json -> Either String CachedTestMaps
decodeCachedTestMaps json = do
  obj <- decodeJson json
  created <-
    obj .: "created"
      >>= Milliseconds
      >>> Instant.instant
      >>> case _ of
          Nothing -> Left "created is not a valid unix timestamp"
          Just created -> Right created
  testMaps <- obj .: "testMaps" >>= traverse decodeJson
  pure { created, testMaps }

-- case mCreated of
--   Nothing -> Left "created is not a valid unix timestamp"
--   Just created -> Right { created, testMaps }
checkIsLessThanDurationAgo :: forall d. Duration d => d -> Instant -> Effect Boolean
checkIsLessThanDurationAgo maxDurationAgo dateTimeInThePast = do
  now <- Now.now >>= Instant.toDateTime >>> pure
  let
    differenceInTime = DateTime.diff now (Instant.toDateTime dateTimeInThePast) :: Milliseconds
  pure $ differenceInTime < (Duration.convertDuration maxDurationAgo)

hashMapFromTestMaps :: Array TestMap -> Map VariantId TestMap
hashMapFromTestMaps = map toKeyValuePair >>> Map.fromFoldable
  where
  toKeyValuePair testMap = Tuple (VariantId testMap.variantId) testMap

getMemoryCachedContext :: ExceptT String Aff TestContext
getMemoryCachedContext = do
  { mCachedTestContext } <- liftEffect MemoryStore.get
  case mCachedTestContext of
    Nothing -> throwError "Cache miss"
    Just cachedTestContext -> do
      isValid <- liftEffect $ checkIsLessThanDurationAgo (Minutes 5.0) cachedTestContext.created
      if isValid then
        pure { variantTestMap: cachedTestContext.variantTestMap }
      else
        throwError "Cache expired"

cachedTestContextKey :: String
cachedTestContextKey = "sweetspot__test_context"

type CachedTestMaps
  = { created :: Instant, testMaps :: Array TestMap }

getLocalStorageCachedContext :: ExceptT String Aff TestContext
getLocalStorageCachedContext =
  liftEffect
    $ HTML.window
    >>= Window.localStorage
    >>= Storage.getItem cachedTestContextKey
    >>= case _ of
        Nothing -> Left "Cache miss"
        Just cachedTestContextJson -> cachedTestContextJson # Argonaut.jsonParser >>= decodeCachedTestMaps
    >>> pure
    >>= case _ of
        Left msg -> throwError (error msg)
        Right cachedTestMaps -> do
          isValid <- checkIsLessThanDurationAgo (Minutes 5.0) cachedTestMaps.created
          when (not isValid) (throwError (error "Cache expired"))
          pure { variantTestMap: (hashMapFromTestMaps cachedTestMaps.testMaps) }

fetchTestContext :: ExceptT String Aff TestContext
fetchTestContext = do
  isRuntimeAdequate <- liftEffect RuntimeDependency.getIsRuntimeAdequate
  when (not isRuntimeAdequate) (throwError inadequateRuntimeError)
  mUserId <- liftEffect User.getUserId
  -- Fetch the list of TestMaps
  -- Cache the list of TestMaps in memory and local storage
  pure { variantTestMap: Map.empty }
  where
  inadequateRuntimeError = "sweetspot can't run in current runtime"

getTestContext :: ExceptT String Aff TestContext
getTestContext =
  -- Are there TestMaps in memory or localstorage and they are not expired?
  getMemoryCachedContext
    <|> getLocalStorageCachedContext
    <|> fetchTestContext

handleExit :: forall a e. Show e => Either e a -> Effect Unit
handleExit = case _ of
  Left message -> Logging.log LogLevel.Error $ show message
  Right _ -> pure unit

testContextFiber :: forall a. Effect (Ref (Aff a))
testContextFiber = Ref.new (Aff.makeAff \_ -> pure Aff.nonCanceler)

main :: Effect Unit
main =
  Aff.runAff_ Console.logShow do
    eTestContext <- runExceptT getTestContext
    case eTestContext of
      Left msg -> throwError (error msg)
      Right testContext -> pure unit

-- Try using Concurrent.BoundedQueue
apply :: Effect Unit
apply = main
