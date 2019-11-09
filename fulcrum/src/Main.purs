module Main where

import Prelude
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Fulcrum.Dom (awaitDomReady) as Dom
import Fulcrum.Logging (LogLevel(..)) as LogLevel
import Fulcrum.Logging (log) as Logging
import Fulcrum.RuntimeDependency (getIsRuntimeAdequate) as RuntimeDependency
import Fulcrum.User (getUserId) as User

type TestContext
  = { skuTestMaps :: Array Unit
    , variantIdTestMaps :: Array Unit
    }

getTestContext :: ExceptT Error Effect TestContext
getTestContext = do
  isRuntimeAdequate <- lift RuntimeDependency.getIsRuntimeAdequate
  when (not isRuntimeAdequate) (throwError $ error inadequateRuntimeError)
  mUserId <- lift User.getUserId
  pure { skuTestMaps: [], variantIdTestMaps: [] }
  where
  inadequateRuntimeError = "sweetspot can't run in current runtime"

handleExit :: forall a e. Show e => Either e a -> Effect Unit
handleExit = case _ of
  Left message -> Logging.log LogLevel.Error $ show message
  Right _ -> pure unit

main :: Effect Unit
main =
  runAff_ handleExit do
    Dom.awaitDomReady
    eTestContext <- liftEffect $ runExceptT getTestContext
    case eTestContext of
      Left msg -> throwError msg
      Right testContext -> pure unit

newtype VariantId
  = VariantId String

apply :: Effect Unit
apply = mempty
