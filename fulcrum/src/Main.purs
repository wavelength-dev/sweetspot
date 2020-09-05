module Fulcrum.Main where

import Prelude
import Control.Monad.Cont (lift)
import Control.Monad.Except (ExceptT, except, runExceptT, throwError)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff, Error, Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.AVar as AAVar
import Effect.Class (liftEffect)
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
import Fulcrum.Site (getIsDebugging, getIsDryRun, getIsPricePage, readHostname) as Site
import Fulcrum.TestPrice (applyTestPrices, revealAllPrices) as TestPrice
import Fulcrum.TestPrice (observeTestPrices)
import Fulcrum.User (UserId)
import Fulcrum.User (findUserId) as User

getTestMap :: UserId -> ExceptT String Aff TestMapByVariant
getTestMap userId = do
  isRuntimeAdequate <- liftEffect RuntimeDependency.getIsRuntimeAdequate
  when (not isRuntimeAdequate) (throwError inadequateRuntimeError)
  testMaps <- Service.fetchTestMaps (OnlyUserId userId) # lift >>= except
  Data.hashMapFromTestMaps testMaps # pure
  where
  inadequateRuntimeError = "sweetspot can't run in current runtime"

handleExit :: forall a e. Show e => Either e a -> Effect Unit
handleExit = case _ of
  Left message -> Logger.log Error $ show message
  Right _ -> mempty

foreign import exposeGlobals :: Effect Unit -> Effect Unit

wrapUp :: Either Error Unit -> Effect Unit
wrapUp result = do
  TestPrice.revealAllPrices
  case result of
    Left error -> Logger.logWithContext Error "main failed" { mainError: error }
    Right _ -> Logger.log Info "succesfully ran sweetspot main loop"

main :: Effect Unit
main =
  Aff.runAff_ wrapUp do
    exposeGlobals reapply # liftEffect
    hostname <- liftEffect Site.readHostname
    isDryRun <- liftEffect Site.getIsDryRun
    isDebugging <- liftEffect Site.getIsDebugging
    awaitDomReady
    isPricePage <- liftEffect Site.getIsPricePage
    when isPricePage do
      Logger.logWithContext Info ("running fulcrum on " <> hostname) { isDryRun, isDebugging } # liftEffect
      eSuccess <-
        runExceptT do
          userId <- findUserIdWithWaitLimit # lift >>= except
          sessionTestContext <-
            liftEffect do
              RunState.initRunQueue
              RunState.initTestContext
              RunState.getTestContext
          testContext <- getTestMap userId
          -- we cache the test maps and apply them
          unless (Map.isEmpty testContext) do
            -- as this is the main loop, and it only runs once, we can safely assume the avar to be empty
            _ <- AAVar.tryPut testContext sessionTestContext # lift
            applyTestMaps testContext # liftEffect
            observeTestPrices testContext # liftEffect
            observeCheckout testContext # liftEffect
      case eSuccess of
        Left msg -> Aff.error msg # throwError
        Right _ -> mempty

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

findUserIdWithWaitLimit :: Aff (Either String UserId)
findUserIdWithWaitLimit = tryFindUserId 0.0
  where
  tryFindUserId waitTime = do
    mUserId <- liftEffect User.findUserId
    case mUserId of
      Just userId -> Right userId # pure
      Nothing ->
        if waitTime > 6000.0 then
          Left "find user id timed out" # pure
        else do
          Aff.delay (Milliseconds 50.0)
          tryFindUserId (waitTime + 50.0)
