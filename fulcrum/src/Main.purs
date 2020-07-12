module Fulcrum.Main where

import Prelude
import Control.Monad.Cont (lift)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff, error)
import Effect.Aff (runAff_, launchAff_) as Aff
import Effect.Aff.AVar as AAVar
import Effect.Class (liftEffect)
import Effect.Timer (setInterval, setTimeout)
import Fulcrum.Cart as Cart
import Fulcrum.Checkout (applyTestCheckout) as Checkout
import Fulcrum.Data (TestMapByVariant)
import Fulcrum.Data (hashMapFromTestMaps) as Data
import Fulcrum.Logger (LogLevel(..))
import Fulcrum.Logger (LogLevel(..)) as LogLevel
import Fulcrum.Logger (log, logWithContext) as Logger
import Fulcrum.RunState (getIsRunning, getRunQueue, getTestContext, initRunQueue, initTestContext, setIsRunning) as RunState
import Fulcrum.RuntimeDependency (getIsRuntimeAdequate) as RuntimeDependency
import Fulcrum.Service (TestMapProvisions(..))
import Fulcrum.Service as Service
import Fulcrum.TestPrice (applyTestPrices) as TestPrice
import Fulcrum.User (UserId)
import Fulcrum.User (findUserId) as User
import Web.HTML (window) as HTML
import Web.HTML.Location as Location
import Web.HTML.Window (location) as Window

getTestMap :: UserId -> ExceptT String Aff TestMapByVariant
getTestMap userId = do
  isRuntimeAdequate <- liftEffect RuntimeDependency.getIsRuntimeAdequate
  when (not isRuntimeAdequate) (throwError inadequateRuntimeError)
  let
    payload = OnlyUserId userId
  eTestMaps <- lift $ Service.fetchTestMaps payload
  case eTestMaps of
    Left msg -> throwError msg
    Right testMaps -> testMaps # Data.hashMapFromTestMaps >>> pure
  where
  inadequateRuntimeError = "sweetspot can't run in current runtime"

handleExit :: forall a e. Show e => Either e a -> Effect Unit
handleExit = case _ of
  Left message -> Logger.log LogLevel.Error $ show message
  Right _ -> mempty

foreign import exposeGlobals :: Effect Unit -> Effect Unit

main :: Effect Unit
main = do
  exposeGlobals reapply
  hostname <- HTML.window >>= Window.location >>= Location.hostname
  Logger.log LogLevel.Info ("running fulcrum on " <> hostname)
  withUserId \userId -> do
    startCartTokenInterval userId
    RunState.initRunQueue
    RunState.initTestContext
    sessionTestContext <- RunState.getTestContext
    Aff.runAff_ logResult do
      eTestContext <- runExceptT $ getTestMap userId
      case eTestContext of
        Left msg -> throwError (error msg)
        -- we cache the test maps and apply them
        Right testContext -> do
          unless (Map.isEmpty testContext) do
            -- as this is the main loop, and it only runs once, we can safely assume the avar to be empty
            _ <- AAVar.tryPut testContext sessionTestContext
            applyTestMaps testContext # liftEffect
  where
  logResult (Left error) = Logger.logWithContext Error "main failed" { error }

  logResult (Right result) = Logger.log Info "succesfully ran sweetspot main loop"

applyTestMaps :: TestMapByVariant -> Effect Unit
applyTestMaps testMap = TestPrice.applyTestPrices testMap *> Checkout.applyTestCheckout testMap

applyDynamicPrice :: Aff Unit
applyDynamicPrice = do
  testContext <- RunState.getTestContext # liftEffect >>= AAVar.read
  mUserId <- User.findUserId # liftEffect
  case mUserId of
    Nothing -> Logger.log Warn "tried to apply prices before userId available" # liftEffect
    Just userId -> applyTestMaps testContext # liftEffect

consumeQueue :: Aff Unit -> Aff Unit
consumeQueue fn = do
  -- Run a queued function.
  liftEffect $ RunState.setIsRunning true
  fn
  -- If there is another function waiting to be consumed by the time we finish, then run it.
  queue <- liftEffect $ RunState.getRunQueue
  mNextFn <- liftEffect $ AVar.tryTake queue
  case mNextFn of
    Nothing -> mempty
    (Just nextFn) -> nextFn

queueNext :: Aff Unit -> Effect Unit
queueNext fn = do
  queue <- RunState.getRunQueue
  queueStatus <- AVar.status queue
  isRunning <- RunState.getIsRunning
  if not isRunning then
    -- Nothing queued, start running
    Aff.runAff_ logResult $ consumeQueue fn
  else
    -- When there is space on the queue, queue. If not, that's fine too, there is no point in queueing more than one run.
    AVar.tryPut (consumeQueue fn) queue # void
  where
  logResult (Left error) = Logger.logWithContext Error "apply failed" { error }

  logResult (Right result) = Logger.log Info "successfully reapplied prices"

reapply :: Effect Unit
reapply = queueNext applyDynamicPrice

startCartTokenInterval :: UserId -> Effect Unit
startCartTokenInterval userId = setInterval 500 cb *> mempty
  where
  cb :: Effect Unit
  cb = do
    mToken <- Cart.findCartToken
    case mToken of
      Just token -> do
        shouldSend <- not <$> Cart.hasCartTokenBeenSent token
        when shouldSend
          $ Aff.launchAff_
          $ Service.sendCartToken userId token
          *> liftEffect (Cart.persistSentToken token)
      Nothing -> mempty

withUserId :: (UserId -> Effect Unit) -> Effect Unit
withUserId f = check
  where
  timeoutMs = 100

  check :: Effect Unit
  check =
    User.findUserId
      >>= case _ of
          Just uid -> f uid
          Nothing -> setTimeout timeoutMs check *> mempty
