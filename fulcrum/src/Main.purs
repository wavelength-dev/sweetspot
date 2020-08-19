module Fulcrum.Main where

import Prelude
import Control.Monad.Cont (lift)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff, Error, Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.AVar as AAVar
import Effect.Class (liftEffect)
import Effect.Exception (try)
import Effect.Timer (setInterval)
import Fulcrum.Cart (findCartToken, hasCartTokenBeenSent, persistSentToken) as Cart
import Fulcrum.Checkout (applyTestCheckout) as Checkout
import Fulcrum.Checkout (observeCheckout)
import Fulcrum.Data (TestMapByVariant)
import Fulcrum.Data (hashMapFromTestMaps) as Data
import Fulcrum.Logger (LogLevel(..))
import Fulcrum.Logger (log, logWithContext) as Logger
import Fulcrum.RunState (getIsRunning, getRunQueue, getTestContext, initRunQueue, initTestContext, setIsRunning) as RunState
import Fulcrum.RuntimeDependency (getIsRuntimeAdequate) as RuntimeDependency
import Fulcrum.Service (TestMapProvisions(..))
import Fulcrum.Service as Service
import Fulcrum.Site (getIsDebugging, getIsDryRun, readHostname) as Site
import Fulcrum.TestPrice (applyTestPrices, revealAllPrices) as TestPrice
import Fulcrum.TestPrice (observeTestPrices)
import Fulcrum.User (UserId)
import Fulcrum.User (findUserId) as User

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
  Left message -> Logger.log Error $ show message
  Right _ -> mempty

foreign import exposeGlobals :: Effect Unit -> Effect Unit

-- regardless of whether we succeeded or failed, we need to unhide the prices so people can buy
withRevealPrices :: forall a. Effect a -> Effect Unit
withRevealPrices fn = try fn *> TestPrice.revealAllPrices

wrapUp :: Either Error Unit -> Effect Unit
wrapUp result = do
  TestPrice.revealAllPrices
  case result of
    Left error -> Logger.logWithContext Error "main failed" { error }
    Right _ -> Logger.log Info "succesfully ran sweetspot main loop"

main :: Effect Unit
main =
  Aff.runAff_ wrapUp do
    liftEffect do
      exposeGlobals reapply
      hostname <- Site.readHostname
      isDryRun <- Site.getIsDryRun
      isDebugging <- Site.getIsDebugging
      Logger.logWithContext Info ("running fulcrum on " <> hostname) { isDryRun, isDebugging }
    eUserId <- findUserIdWithWaitLimit
    case eUserId of
      Left msg -> throwError $ Aff.error msg
      Right userId -> do
        sessionTestContext <-
          liftEffect do
            startCartTokenInterval userId
            RunState.initRunQueue
            RunState.initTestContext
            RunState.getTestContext
        eTestContext <- runExceptT $ getTestMap userId
        case eTestContext of
          Left msg -> throwError (Aff.error msg)
          -- we cache the test maps and apply them
          Right testContext -> do
            unless (Map.isEmpty testContext) do
              -- as this is the main loop, and it only runs once, we can safely assume the avar to be empty
              _ <- AAVar.tryPut testContext sessionTestContext
              applyTestMaps testContext # liftEffect
              observeTestPrices testContext # liftEffect
              observeCheckout testContext # liftEffect

applyTestMaps :: TestMapByVariant -> Effect Unit
applyTestMaps testMap = TestPrice.applyTestPrices testMap *> Checkout.applyTestCheckout testMap

applyDynamicPrice :: Aff Unit
applyDynamicPrice = do
  testContext <- RunState.getTestContext # liftEffect >>= AAVar.read
  applyTestMaps testContext # liftEffect

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
          $ Aff.launchAff_ do
              eSuccess <- Service.sendCartToken userId token
              liftEffect case eSuccess of
                Left errMsg -> Logger.log Error errMsg
                Right _ -> Cart.persistSentToken token
      Nothing -> mempty

findUserIdWithWaitLimit :: Aff (Either String UserId)
findUserIdWithWaitLimit =
  let
    tryFindUserId waitTime = do
      mUserId <- liftEffect User.findUserId
      case mUserId of
        Just userId -> pure $ Right userId
        Nothing ->
          if waitTime > 6000.0 then
            "find user id timed out" # Left >>> pure
          else do
            Aff.delay (Milliseconds 50.0)
            tryFindUserId (waitTime + 50.0)
  in
    tryFindUserId 0.0
