module Fulcrum.Site where

import Prelude
import Data.Array (catMaybes, concat, head, null) as Array
import Data.Either (Either(..))
import Data.Foldable (oneOf) as Foldable
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Fulcrum.Config (dryRunMap) as Config
import Prim.Row (class Union)
import QueryString (QueryParam(..))
import QueryString (parseQueryString) as QueryString
import Web.DOM (Document, Element)
import Web.DOM.Document (toParentNode) as Document
import Web.DOM.Element (fromNode, toNode) as Element
import Web.DOM.MutationObserver (MutationObserverInitFields)
import Web.DOM.MutationObserver (mutationObserver, observe) as MutationObserver
import Web.DOM.MutationRecord (MutationRecord)
import Web.DOM.MutationRecord (target) as MutationRecord
import Web.DOM.NodeList (toArray) as NodeList
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.ParentNode (querySelectorAll) as ParentNode
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener) as EventTarget
import Web.HTML (window) as HTML
import Web.HTML.Event.EventTypes (domcontentloaded) as EventTypes
import Web.HTML.HTMLDocument (readyState, toDocument) as HTMLDocument
import Web.HTML.HTMLDocument.ReadyState (ReadyState(..))
import Web.HTML.Location (hostname, search) as Location
import Web.HTML.Window as Window

foreign import getIsDebugSession :: Effect Boolean

foreign import setIsDebugSession :: Boolean -> Effect Unit

getDocument :: Effect Document
getDocument = HTML.window >>= Window.document >>= HTMLDocument.toDocument >>> pure

awaitDomReady :: Aff Unit
awaitDomReady =
  makeAff \callback -> do
    rs <- HTMLDocument.readyState =<< Window.document =<< HTML.window
    case rs of
      Loading -> do
        eventTarget <- Window.toEventTarget <$> HTML.window
        listener <- EventTarget.eventListener (\_ -> callback (Right unit))
        EventTarget.addEventListener EventTypes.domcontentloaded listener false eventTarget
        pure $ effectCanceler (EventTarget.removeEventListener EventTypes.domcontentloaded listener false eventTarget)
      _ -> do
        callback (Right unit)
        pure nonCanceler

getUrlParam :: String -> Effect (Maybe String)
getUrlParam targetKey = do
  queryString <- HTML.window >>= Window.location >>= Location.search
  queryString
    # QueryString.parseQueryString
    >>> map matchQueryParam
    >>> Foldable.oneOf
    >>> pure
  where
  matchQueryParam :: Either String QueryParam -> Maybe String
  matchQueryParam = case _ of
    Right (QueryParam key (Just value))
      | key == targetKey -> Just value
    _ -> Nothing

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

findElement :: Array QuerySelector -> Effect (Maybe Element)
findElement querySelectors =
  traverse queryDocument querySelectors
    <#> Array.concat
    >>> Array.head

getIsDebugging :: Effect Boolean
getIsDebugging = do
  mUrlDebugParam <- getUrlParam "ssdebug"
  case mUrlDebugParam of
    -- URL says nothing, fallback to session setting
    Nothing -> getIsDebugSession
    -- URL says something, store for session and use that
    Just urlDebugParam -> do
      let
        shouldDebug = urlDebugParam == "true"
      setIsDebugSession shouldDebug
      pure shouldDebug

readHostname :: Effect String
readHostname =
  HTML.window
    >>= Window.location
    >>= Location.hostname

getIsDryRun :: Effect Boolean
getIsDryRun = map Config.dryRunMap readHostname

onElementsMutation ::
  forall r rx.
  Union r rx MutationObserverInitFields =>
  Record r -> (Array Element -> Effect Unit) -> Array Element -> Effect Unit
onElementsMutation options callback elements = do
  mutationObserver <- liftEffect $ MutationObserver.mutationObserver (\mrs _ -> mutationRecordsToElements mrs >>= callback)
  let
    observe = Element.toNode >>> \node -> MutationObserver.observe node options mutationObserver
  traverse_ observe elements
  where
  -- We discard the possibilty of some observed nodes not being elements as the only nodes we watch are price elements which are necessarily Html elements.
  mutationRecordsToElements :: Array MutationRecord -> Effect (Array Element)
  mutationRecordsToElements mutationRecords =
    traverse (MutationRecord.target >>> liftEffect >=> Element.fromNode >>> pure) mutationRecords
      >>= Array.catMaybes
      >>> pure

getIsPricePage :: Effect Boolean
getIsPricePage =
  queryDocument (QuerySelector "[data-sweetspot-id]")
    <#> Array.null
    >>> not
