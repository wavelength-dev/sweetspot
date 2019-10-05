module SweetSpot.SiteCapabilities.Dom (awaitDomReady, queryDocument_, removeClass) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, makeAff, nonCanceler)
import Web.DOM (Element)
import Web.DOM.DOMTokenList as DTL
import Web.DOM.Document (toParentNode) as Document
import Web.DOM.Element as Element
import Web.DOM.NodeList (toArray) as NodeList
import Web.DOM.ParentNode (QuerySelector)
import Web.DOM.ParentNode (querySelectorAll) as ParentNode
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (HTMLElement, window)
import Web.HTML.Event.EventTypes as ET
import Web.HTML.HTMLDocument (readyState, toDocument) as HTMLDocument
import Web.HTML.HTMLDocument.ReadyState (ReadyState(..))
import Web.HTML.HTMLElement (classList)
import Web.HTML.Window as Window

removeClass :: String -> HTMLElement -> Effect Unit
removeClass className = classList >=> (flip DTL.remove) className

awaitDomReady :: Aff Unit
awaitDomReady =
  makeAff \callback -> do
    rs <- HTMLDocument.readyState =<< Window.document =<< window
    case rs of
      Loading -> do
        et <- Window.toEventTarget <$> window
        listener <- eventListener (\_ -> callback (Right unit))
        addEventListener ET.domcontentloaded listener false et
        pure $ effectCanceler (removeEventListener ET.domcontentloaded listener false et)
      _ -> do
        callback (Right unit)
        pure nonCanceler

queryDocument_ :: QuerySelector -> Effect (Array Element)
queryDocument_ querySelector =
  window
    >>= Window.document
    >>= HTMLDocument.toDocument
    >>> Document.toParentNode
    >>> pure
    >>= ParentNode.querySelectorAll querySelector
    >>= nodesToElements
  where
  -- We discard nodes that are not elements.
  nodesToElements = NodeList.toArray >=> map Element.fromNode >>> Array.catMaybes >>> pure
