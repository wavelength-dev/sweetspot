module Fulcrum.Main where

import Prelude
import Control.Monad.Cont (lift)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map (fromFoldable, lookup) as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Datadog as Logger
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff, error)
import Effect.Aff (runAff_, launchAff_) as Aff
import Effect.Class (liftEffect)
import Effect.Console (error, log, logShow) as Console
import Effect.Timer (setInterval, setTimeout)
import Fulcrum.Cart as Cart
import Fulcrum.Data (TestMap, VariantId(..))
import Fulcrum.Logging (LogLevel(..)) as LogLevel
import Fulcrum.Logging (log, logWithContext) as Logging
import Fulcrum.RunState (getIsRunning, getRunQueue, initRunQueue, setIsRunning) as RunState
import Fulcrum.RuntimeDependency (getIsRuntimeAdequate) as RuntimeDependency
import Fulcrum.Service (TestMapProvisions(..))
import Fulcrum.Service as Service
import Fulcrum.User (UserId)
import Fulcrum.User (findUserId) as User
import Web.DOM (Element)
import Web.DOM.DOMTokenList as DTL
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.HTMLCollection as HTMLCollection
import Web.DOM.Node as Node
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.ParentNode as ParentNode
import Web.HTML (window) as HTML
import Web.HTML.HTMLDocument (toDocument) as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Location as Location
import Web.HTML.Window (document, location) as Window

type TestMapByVariant
  = Map VariantId TestMap

hashMapFromTestMaps :: Array TestMap -> Map VariantId TestMap
hashMapFromTestMaps = map toKeyValuePair >>> Map.fromFoldable
  where
  toKeyValuePair testMap = Tuple (VariantId testMap.variantId) testMap

getTestMap :: UserId -> ExceptT String Aff TestMapByVariant
getTestMap userId = do
  isRuntimeAdequate <- liftEffect RuntimeDependency.getIsRuntimeAdequate
  when (not isRuntimeAdequate) (throwError inadequateRuntimeError)
  let
    payload = OnlyUserId userId
  eTestMaps <- lift $ Service.fetchTestMaps payload
  case eTestMaps of
    Left msg -> throwError msg
    Right testMaps -> testMaps # hashMapFromTestMaps >>> pure
  where
  inadequateRuntimeError = "sweetspot can't run in current runtime"

handleExit :: forall a e. Show e => Either e a -> Effect Unit
handleExit = case _ of
  Left message -> Logging.log LogLevel.Error $ show message
  Right _ -> mempty

main :: Effect Unit
main = do
  hostname <- HTML.window >>= Window.location >>= Location.hostname
  Logging.log LogLevel.Info ("Running Fulcrum on " <> hostname)
  withUserId
    $ \userId -> do
        startCartTokenInterval userId
        RunState.initRunQueue
        Aff.runAff_ logResult do
          eTestContext <- runExceptT $ getTestMap userId
          case eTestContext of
            Left msg -> throwError (error msg)
            -- We do nothing here as our only goal is to cache the test maps
            Right testContext -> liftEffect $ applyTestMaps testContext
  where
  logResult (Left error) = Logging.logWithContext LogLevel.Error "Main failed" { error }

  logResult (Right result) = mempty

insertPrice :: TestMapByVariant -> Element -> Effect Unit
insertPrice testMap element = do
  mVariantId <- Element.getAttribute "data-sweetspot-id" element
  rawVariantToMaybe mVariantId
    >>= lookupF testMap
    # case _ of
        Nothing -> revealPrice
        Just test -> setNodePrice test *> revealPrice
  where
  lookupF = flip Map.lookup

  rawVariantToMaybe :: Maybe String -> Maybe VariantId
  rawVariantToMaybe rawVariantId = rawVariantId <#> VariantId

  setNodePrice :: TestMap -> Effect Unit
  setNodePrice { swapPrice } = Node.setTextContent swapPrice (Element.toNode element)

  revealPrice :: Effect Unit
  revealPrice = case HTMLElement.fromElement element of
    Just el -> HTMLElement.classList el >>= (flip DTL.remove) "sweetspot__price--hidden"
    Nothing -> Console.error "Unable to reveal price"

applyTestPrices :: Map VariantId TestMap -> Effect Unit
applyTestPrices testMap =
  HTML.window
    >>= Window.document
    >>= HTMLDocument.toDocument
    >>> pure
    >>= Document.getElementsByClassName "sweetspot__price"
    >>= HTMLCollection.toArray
    >>= traverse_ (insertPrice testMap)

queryDocument :: QuerySelector -> Effect (Array Element)
queryDocument querySelector =
  HTML.window
    >>= Window.document
    >>= HTMLDocument.toDocument
    >>> Document.toParentNode
    >>> pure
    >>= ParentNode.querySelectorAll querySelector
    >>= nodesToElements
  where
  -- We discard nodes that are not elements.
  nodesToElements = NodeList.toArray >=> map Element.fromNode >>> Array.catMaybes >>> pure

setCheckoutVariantId :: TestMapByVariant -> Element -> Effect Unit
setCheckoutVariantId testMap element = do
  (eApplied :: Either String Unit) <-
    runExceptT do
      (mValue :: Maybe String) <- Element.getAttribute "value" element # liftEffect
      (targetVariantId :: String) <- case mValue of
        Nothing -> throwError "Checkout option missing value attribute" :: ExceptT String Effect String
        Just targetVariantId -> pure targetVariantId
      case Map.lookup (VariantId targetVariantId) testMap of
        -- Variant is not under test
        Nothing -> pure unit
        Just test -> Element.setAttribute "value" test.swapId element # liftEffect
  case eApplied of
    Left err -> Logger.logError err
    _ -> mempty

applyTestCheckout :: TestMapByVariant -> Effect Unit
applyTestCheckout testMap = do
  optionElements <- queryDocument (QuerySelector "#ProductSelect-product-template option")
  traverse_ (setCheckoutVariantId testMap) optionElements
  where
  -- We discard nodes that are not elements.
  nodesToElements = NodeList.toArray >=> map Element.fromNode >>> Array.catMaybes >>> pure

applyTestMaps :: TestMapByVariant -> Effect Unit
applyTestMaps testMap = applyTestPrices testMap *> applyTestCheckout testMap

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
      (Just token) -> do
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
  check = do
    mUserId <- liftEffect User.findUserId
    case mUserId of
      Just uid -> f uid
      Nothing -> setTimeout timeoutMs check *> mempty
