module Fulcrum.Main where

import Prelude
import Control.Monad.Cont (lift)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff, error)
import Effect.Aff (runAff_, launchAff_) as Aff
import Effect.Class (liftEffect)
import Effect.Console (log, logShow) as Console
import Effect.Timer (setInterval, setTimeout)
import Fulcrum.Cart as Cart
import Fulcrum.Checkout (applyTestCheckout) as Checkout
import Fulcrum.Data (TestMapByVariant)
import Fulcrum.Data (hashMapFromTestMaps) as Data
import Fulcrum.Logger (LogLevel(..)) as LogLevel
import Fulcrum.Logger (log, logWithContext) as Logging
import Fulcrum.RunState (getIsRunning, getRunQueue, initRunQueue, setIsRunning) as RunState
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
  Left message -> Logging.log LogLevel.Error $ show message
  Right _ -> mempty

main :: Effect Unit
main = do
  hostname <- HTML.window >>= Window.location >>= Location.hostname
  Logging.log LogLevel.Info ("running fulcrum on " <> hostname)
  withUserId \userId -> do
    startCartTokenInterval userId
    RunState.initRunQueue
    Aff.runAff_ logResult do
      eTestContext <- runExceptT $ getTestMap userId
      case eTestContext of
        Left msg -> throwError (error msg)
        -- We do nothing here as our only goal is to cache the test maps
        Right testContext -> liftEffect $ applyTestMaps testContext
  where
  logResult (Left error) = Logging.logWithContext LogLevel.Error "main failed" { error }

  logResult (Right result) = mempty

applyTestMaps :: TestMapByVariant -> Effect Unit
applyTestMaps testMap = TestPrice.applyTestPrices testMap *> Checkout.applyTestCheckout testMap

applyDynamicPrice :: Aff Unit
applyDynamicPrice = liftEffect $ Console.log "Applying price things!"

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
    Aff.runAff_ Console.logShow $ consumeQueue fn
  else
    -- When there is space on the queue, queue. If not, that's fine too, there is no point in queueing more than one run.
    AVar.tryPut (consumeQueue fn) queue # void

reapply :: Effect Unit
reapply = queueNext applyDynamicPrice

startCartTokenInterval :: UserId -> Effect Unit
startCartTokenInterval userId = setInterval (1000 * 3) cb *> mempty
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
