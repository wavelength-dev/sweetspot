module Fulcrum.Main where

import Prelude
import Control.Monad.Cont (lift)
import Control.Monad.Except (except, runExceptT, throwError)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff, Error, Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.AVar as AAVar
import Effect.Class (liftEffect)
import Effect.Exception (catchException)
import Fulcrum.Checkout (setTestCheckout, registerOnSelectVariant) as Checkout
import Fulcrum.Data (TestMapByVariant)
import Fulcrum.Logger (LogLevel(..))
import Fulcrum.Logger (log, logWithContext) as Logger
import Fulcrum.RunState (getIsRunning, getRunQueue, getTestContext, initRunQueue, initTestContext, setIsRunning) as RunState
import Fulcrum.RuntimeDependency (getIsRuntimeAdequate) as RuntimeDependency
import Fulcrum.Site (awaitDomReady)
import Fulcrum.Site (getIsDebugging, getIsDryRun, getIsPricePage, readHostname) as Site
import Fulcrum.TestMap (getTestMap) as TestMap
import Fulcrum.TestPrice (applyTestPrices, observeTestPrices, revealAllPrices) as TestPrice
import Fulcrum.User (UserId)
import Fulcrum.User (findUserId) as User

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

-- Sites that we run on have tons of unhandled exceptions. Becuase of
-- this we can't catch all errors that are unhandled as many aren't
-- ours. We turn off forwarding of all unhandled errors on the window
-- . Instead we wrap our logic with our own unhandled handler.
withHandledExceptions :: Effect Unit -> Effect Unit
withHandledExceptions =
  catchException \error ->
    Logger.logWithContext Error "unhandled exception" error

main :: Effect Unit
main =
  withHandledExceptions
    $ Aff.runAff_ wrapUp do
        exposeGlobals reapply # liftEffect
        hostname <- liftEffect Site.readHostname
        isDryRun <- liftEffect Site.getIsDryRun
        isDebugging <- liftEffect Site.getIsDebugging
        awaitDomReady
        Logger.logWithContext Info ("running fulcrum on " <> hostname) { isDryRun, isDebugging } # liftEffect
        eSuccess <-
          runExceptT do
            isRuntimeAdequate <- liftEffect RuntimeDependency.getIsRuntimeAdequate
            unless isRuntimeAdequate (throwError "browser runtime inadequate for sweetspot")
            userId <- findUserIdWithWaitLimit # lift >>= except
            sessionTestContext <-
              liftEffect do
                RunState.initRunQueue
                RunState.initTestContext
                RunState.getTestContext
            testContext <- TestMap.getTestMap userId
            isPricePage <- liftEffect Site.getIsPricePage
            when isPricePage do
              unless (Map.isEmpty testContext) do
                -- we cache the test maps and apply them
                -- as this is the main loop, and it only runs once, we can safely assume the avar to be empty
                _ <- AAVar.tryPut testContext sessionTestContext # lift
                applyTestMaps testContext # liftEffect
                TestPrice.observeTestPrices testContext # liftEffect
                Checkout.registerOnSelectVariant testContext # liftEffect
        case eSuccess of
          Left msg -> Logger.log Error msg # liftEffect
          Right _ -> mempty

applyTestMaps :: TestMapByVariant -> Effect Unit
applyTestMaps testMap = TestPrice.applyTestPrices testMap *> Checkout.setTestCheckout testMap

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
reapply = withHandledExceptions $ queueNext applyDynamicPrice

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
