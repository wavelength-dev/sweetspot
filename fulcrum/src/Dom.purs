module Fulcrum.Dom where

import Prelude

import Data.Array (catMaybes) as Array
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, makeAff, nonCanceler)
import Web.DOM (Element)
import Web.DOM.Document (toParentNode) as Document
import Web.DOM.Element (fromNode) as Element
import Web.DOM.NodeList (toArray) as NodeList
import Web.DOM.ParentNode (QuerySelector)
import Web.DOM.ParentNode (querySelectorAll) as ParentNode
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener) as EventTarget
import Web.HTML (window) as HTML
import Web.HTML.Event.EventTypes (domcontentloaded) as EventTypes
import Web.HTML.HTMLDocument (readyState, toDocument) as HTMLDocument
import Web.HTML.HTMLDocument.ReadyState (ReadyState(..))
import Web.HTML.Window as Window

awaitDomReady :: Aff Unit
awaitDomReady =
  makeAff \callback -> do
    rs <- HTMLDocument.readyState =<< Window.document =<< HTML.window
    case rs of
      Loading -> do
        et <- Window.toEventTarget <$> HTML.window
        listener <- EventTarget.eventListener (\_ -> callback (Right unit))
        EventTarget.addEventListener EventTypes.domcontentloaded listener false et
        pure $ effectCanceler (EventTarget.removeEventListener EventTypes.domcontentloaded listener false et)
      _ -> do
        callback (Right unit)
        pure nonCanceler

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
