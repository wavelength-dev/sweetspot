module Fulcrum.TestPrice where

import Prelude
import Data.Array as Array
import Data.Map (Map)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Traversable (traverse_)
import Effect (Effect)
import Fulcrum.Data (TestMapByVariant, VariantId(..), TestMap)
import Fulcrum.Logger (LogLevel(..))
import Fulcrum.Logger (log) as Logger
import Fulcrum.Site as Site
import Partial.Unsafe (unsafePartial) as Unsafe
import Web.DOM (Element)
import Web.DOM.DOMTokenList (remove) as DTL
import Web.DOM.Document (createElement, toParentNode) as Document
import Web.DOM.Element (fromNode, getAttribute, setAttribute, toNode) as Element
import Web.DOM.Node (appendChild, insertBefore, nextSibling, parentNode, setTextContent) as Node
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.ParentNode as ParentNode
import Web.HTML (window) as HTML
import Web.HTML.HTMLDocument (toDocument) as HTMLDocument
import Web.HTML.HTMLElement (classList, fromElement) as HTMLElement
import Web.HTML.Window (document) as Window

highlightTestPrice :: Element -> Boolean -> Effect Unit
highlightTestPrice priceElement isTest = do
  -- if highlight exists, skip
  elements <- Site.queryDocument (QuerySelector ".sweetspot__price--highlight")
  when ((Array.length elements) == 0) do
    let
      text = if isTest then "test price" else "default price"
    document <- HTML.window >>= Window.document <#> HTMLDocument.toDocument
    Element.setAttribute "style" "color: #5a31f4;" priceElement
    labelElement <- Document.createElement "sup" document
    Element.setAttribute "style" "color: #5a31f4;" labelElement
    Element.setAttribute "class" "sweetspot__price--highlight" labelElement
    let
      priceNode = Element.toNode priceElement

      labelNode = Element.toNode labelElement
    Node.setTextContent text labelNode
    parentNode <- Node.parentNode priceNode <#> Unsafe.unsafePartial Maybe.fromJust
    mSiblingNode <- Node.nextSibling priceNode
    case mSiblingNode of
      Nothing ->
        Node.appendChild labelNode parentNode
        *> mempty
      Just siblingNode ->
        Node.insertBefore labelNode siblingNode parentNode
        *> mempty

insertPrice :: TestMapByVariant -> Element -> Effect Unit
insertPrice testMap element = do
  isDebugging <- Site.getIsDebugging
  isDryRun <- Site.getIsDryRun
  mUrlVariantId <- Site.getUrlParam "variant" <#> (map VariantId)
  -- this id is only valid when no variant is selected
  elementVariantId <-
    -- as we select elements by this attribute we can be sure the
    -- attribute is there
    Element.getAttribute "data-sweetspot-id" element
      <#> (Unsafe.unsafePartial Maybe.fromJust)
      >>> VariantId
  -- if there is a variant query parameter, we're on a page with a
  -- selected variant and we use that id
  let
    variantId = Maybe.fromMaybe elementVariantId mUrlVariantId
  case Map.lookup variantId testMap of
    -- price not under test, nothing to do
    Nothing -> mempty
    Just test -> do
      unless isDryRun do
        setNodePrice test
      when isDebugging do
        let
          isTestPrice = test.variantId /= test.swapId
        (highlightTestPrice element isTestPrice)
      when (isDebugging && isDryRun) do
        setNodePrice test
  where
  setNodePrice :: TestMap -> Effect Unit
  setNodePrice { swapPrice } = Node.setTextContent swapPrice (Element.toNode element)

revealPrice :: Element -> Effect Unit
revealPrice element = case HTMLElement.fromElement element of
  Just el -> HTMLElement.classList el >>= (flip DTL.remove) "sweetspot__price--hidden"
  Nothing -> Logger.log Error "unable to reveal price"

revealAllPrices :: Effect Unit
revealAllPrices =
  Site.queryDocument (QuerySelector ".sweetspot__price--hidden")
    >>= traverse_ revealPrice

applyTestPrices :: Map VariantId TestMap -> Effect Unit
applyTestPrices testMap =
  HTML.window
    >>= Window.document
    <#> HTMLDocument.toDocument
    <#> Document.toParentNode
    >>= ParentNode.querySelectorAll (QuerySelector "[data-sweetspot-id]")
    >>= NodeList.toArray
    <#> map Element.fromNode
    <#> Array.catMaybes
    >>= traverse_ (insertPrice testMap)

observeTestPrices :: Map VariantId TestMap -> Effect Unit
observeTestPrices testMap =
  HTML.window
    >>= Window.document
    <#> HTMLDocument.toDocument
    <#> Document.toParentNode
    >>= ParentNode.querySelectorAll (QuerySelector "[data-sweetspot-id]")
    >>= NodeList.toArray
    <#> map Element.fromNode
    <#> Array.catMaybes
    >>= Site.onElementsMutation
        { characterData: true, childList: true }
        (traverse_ (insertPrice testMap))
