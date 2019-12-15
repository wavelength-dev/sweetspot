module Main where

import Prelude

import Control.Monad.Cont (lift)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Either (Either(..), note)
import Data.Map (Map)
import Data.Map (fromFoldable, lookup) as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff, error)
import Effect.Aff (runAff_) as Aff
import Effect.Class (liftEffect)
import Effect.Console (error, log, logShow) as Console
import Fulcrum.Data (TestMap, VariantId(..))
import Fulcrum.Logging (LogLevel(..)) as LogLevel
import Fulcrum.Logging (log) as Logging
import Fulcrum.RunState (getRunQueue, initRunQueue) as RunState
import Fulcrum.RuntimeDependency (getIsRuntimeAdequate) as RuntimeDependency
import Fulcrum.Service (TestMapProvisions(..))
import Fulcrum.Service as Service
import Fulcrum.User (UserId(..))
import Fulcrum.User (getUserId) as User
import Web.DOM (Element)
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.HTMLCollection as HTMLCollection
import Web.DOM.Node as Node
import Web.HTML (window) as HTML
import Web.HTML.HTMLDocument (toDocument) as HTMLDocument
import Web.HTML.Window (document) as Window

type TestMapByVariant
  = Map VariantId TestMap

hashMapFromTestMaps :: Array TestMap -> Map VariantId TestMap
hashMapFromTestMaps = map toKeyValuePair >>> Map.fromFoldable
  where
  toKeyValuePair testMap = Tuple (VariantId testMap.variantId) testMap

getTestMap :: ExceptT String Aff TestMapByVariant
getTestMap = do
  isRuntimeAdequate <- liftEffect RuntimeDependency.getIsRuntimeAdequate
  when (not isRuntimeAdequate) (throwError inadequateRuntimeError)
  mUserId <- liftEffect User.getUserId
  -- Fetch the list of TestMaps
  eTestMaps <- lift $ Service.fetchTestMaps (OnlyUserId (UserId "9"))
  -- TODO: Cache the list of TestMaps in memory and local storage
  case eTestMaps of
       Left msg -> throwError msg
       Right testMaps -> testMaps # hashMapFromTestMaps >>> pure
  where
  inadequateRuntimeError = "sweetspot can't run in current runtime"

handleExit :: forall a e. Show e => Either e a -> Effect Unit
handleExit = case _ of
  Left message -> Logging.log LogLevel.Error $ show message
  Right _ -> pure unit

main :: Effect Unit
main = do
  RunState.initRunQueue
  Aff.runAff_ Console.logShow do
    eTestContext <- runExceptT getTestMap
    case eTestContext of
      Left msg -> throwError (error msg)
      Right testContext -> pure unit

insertPrice :: TestMapByVariant -> Element -> Effect Unit
insertPrice testMap element = do
  mVariantId <- Element.getAttribute "data-sweetspot-id" element
  let
    eTestMap =
      rawVariantToEither mVariantId
        >>= (lookupF testMap >>> note "No test for read variant id")
  case eTestMap of
    Left msg -> Console.error msg -- pure $ Left msg
    Right test -> setNodePrice test
  where
  lookupF = flip Map.lookup

  rawVariantToEither :: Maybe String -> Either String VariantId
  rawVariantToEither rawVariantId = note "Missing variant id" rawVariantId <#> VariantId

  setNodePrice :: TestMap -> Effect Unit
  setNodePrice { swapPrice } = Node.setTextContent (show swapPrice) (Element.toNode element)

applyTestMaps :: TestMapByVariant -> Effect Unit
applyTestMaps testMap =
  HTML.window
    >>= Window.document
    >>= HTMLDocument.toDocument
    >>> pure
    >>= Document.getElementsByClassName "sweetspot__price"
    >>= HTMLCollection.toArray
    >>= traverse_ (insertPrice testMap)

applyDynamicPrice :: Aff Unit
applyDynamicPrice = liftEffect $ Console.log "Applying price things!"

consumeQueue :: Aff Unit -> Aff Unit
consumeQueue fn = do
  -- Run a queued function.
  fn :: Aff Unit
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
  if not $ AVar.isFilled queueStatus then
    -- Nothing queued, start running
    Aff.runAff_ Console.logShow $ consumeQueue fn
  else
    -- Try to queue the next run, if we succeed great, if we fail, something is queued, and we queue at most one, so great too, we're done.
    AVar.tryPut (consumeQueue fn) queue # void

reapply :: Effect Unit
reapply = queueNext applyDynamicPrice
