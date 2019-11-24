module Main where

import Prelude
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map (empty, fromFoldable) as Map
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff (makeAff, nonCanceler, runAff_) as Aff
import Effect.Class (liftEffect)
import Effect.Console (logShow) as Console
import Effect.Exception (error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Fulcrum.Data (TestMap, VariantId(..))
import Fulcrum.Logging (LogLevel(..)) as LogLevel
import Fulcrum.Logging (log) as Logging
import Fulcrum.RuntimeDependency (getIsRuntimeAdequate) as RuntimeDependency
import Fulcrum.User (getUserId) as User

type TestContext
  = { variantTestMap :: Map VariantId TestMap }

hashMapFromTestMaps :: Array TestMap -> Map VariantId TestMap
hashMapFromTestMaps = map toKeyValuePair >>> Map.fromFoldable
  where
  toKeyValuePair testMap = Tuple (VariantId testMap.variantId) testMap

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
getTestContext = fetchTestContext

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
