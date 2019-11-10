module Main where

import Prelude
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff (makeAff, nonCanceler, runAff) as Aff
import Effect.Class (liftEffect)
import Effect.Console (log) as Console
import Effect.Exception (Error, error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Fulcrum.Logging (LogLevel(..)) as LogLevel
import Fulcrum.Logging (log) as Logging
import Fulcrum.RuntimeDependency (getIsRuntimeAdequate) as RuntimeDependency
import Fulcrum.Site (awaitDomReady, getDocument) as Site
import Fulcrum.User (getUserId) as User
import Web.DOM.Document (getElementsByClassName) as Document
import Web.DOM.HTMLCollection (length) as HTMLCollection

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

testContextFiber :: forall a. Effect (Ref (Aff a))
testContextFiber = Ref.new (Aff.makeAff \_ -> pure Aff.nonCanceler)

main :: Effect Unit
main = do
  fib <-
    Aff.runAff handleExit do
      Site.awaitDomReady
      eTestContext <- liftEffect $ runExceptT getTestContext
      case eTestContext of
        Left msg -> throwError msg
        Right testContext -> pure unit
  mempty

newtype VariantId
  = VariantId String

apply :: Effect Unit
apply = do
  document <- Site.getDocument
  priceElements <- Document.getElementsByClassName "sweetspot__price" document
  numElements <- HTMLCollection.length priceElements
  Console.log $ "Elements found: " <> show numElements
