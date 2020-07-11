module Fulcrum.TestPrice where

import Prelude
import Data.Map (Map)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Traversable (traverse_)
import Effect (Effect)
import Fulcrum.Data (TestMapByVariant, VariantId(..), TestMap)
import Fulcrum.Logger (LogLevel(..), getIsDebugging)
import Fulcrum.Logger (log) as Logger
import Partial.Unsafe (unsafePartial) as Unsafe
import Web.DOM (Element)
import Web.DOM.DOMTokenList (remove) as DTL
import Web.DOM.Document (createElement, getElementsByClassName) as Document
import Web.DOM.Element (getAttribute, setAttribute, toNode) as Element
import Web.DOM.HTMLCollection (toArray) as HTMLCollection
import Web.DOM.Node (appendChild, parentNode, setTextContent) as Node
import Web.HTML (window) as HTML
import Web.HTML.HTMLDocument (toDocument) as HTMLDocument
import Web.HTML.HTMLElement (classList, fromElement) as HTMLElement
import Web.HTML.Window (document) as Window

highlightTestPrice :: Element -> String -> Effect Unit
highlightTestPrice priceElement text = do
  document <- HTML.window >>= Window.document >>= HTMLDocument.toDocument >>> pure
  Element.setAttribute "style" "color: #5a31f4;" priceElement
  labelElement <- Document.createElement "sup" document
  Element.setAttribute "style" "color: #5a31f4;" labelElement
  let
    priceNode = Element.toNode priceElement

    labelNode = Element.toNode labelElement
  Node.setTextContent text labelNode
  parentNode <- Node.parentNode priceNode <#> Unsafe.unsafePartial Maybe.fromJust
  Node.appendChild labelNode parentNode *> mempty

insertPrice :: TestMapByVariant -> Element -> Effect Unit
insertPrice testMap element = do
  isDebugging <- getIsDebugging
  mVariantId <- Element.getAttribute "data-sweetspot-id" element <#> (map VariantId)
  case mVariantId of
    Nothing -> do
      Logger.log Warn "sweetspot price without variant id"
      when isDebugging (highlightTestPrice element "error")
    Just variantId -> case Map.lookup variantId testMap of
      -- sweetspot price but price not under test
      Nothing -> revealPrice
      Just test -> do
        setNodePrice test
        revealPrice
        let
          highlightText = if test.variantId == test.swapId then "default price" else "test price"
        when isDebugging (highlightTestPrice element highlightText)
  where
  setNodePrice :: TestMap -> Effect Unit
  setNodePrice { swapPrice } = Node.setTextContent swapPrice (Element.toNode element)

  revealPrice :: Effect Unit
  revealPrice = case HTMLElement.fromElement element of
    Just el -> HTMLElement.classList el >>= (flip DTL.remove) "sweetspot__price--hidden"
    Nothing -> Logger.log Error "unable to reveal price"

applyTestPrices :: Map VariantId TestMap -> Effect Unit
applyTestPrices testMap =
  HTML.window
    >>= Window.document
    >>= HTMLDocument.toDocument
    >>> pure
    >>= Document.getElementsByClassName "sweetspot__price"
    >>= HTMLCollection.toArray
    >>= traverse_ (insertPrice testMap)
