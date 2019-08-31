module SweetSpot.SiteCapabilities where

import Prelude

import Data.Array (catMaybes, find) as Array
import Data.Maybe (Maybe)
import Effect (Effect)
import SweetSpot.Data.Api (TestMap)
import Web.DOM (Element, NodeList)
import Web.DOM.Document (toParentNode) as Document
import Web.DOM.Element (fromNode, getAttribute, setAttribute) as Element
import Web.DOM.NodeList (toArray) as NodeList
import Web.DOM.ParentNode (QuerySelector, querySelectorAll)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document) as Window

class Monad m <= DomAction m where
  getAttribute :: String -> Element -> m (Maybe String)
  setAttribute :: String -> String -> Element -> m Unit
  queryDocument :: QuerySelector -> m (Array Element)

instance domActionEffect :: DomAction Effect where
  getAttribute = Element.getAttribute
  setAttribute = Element.setAttribute
  queryDocument = queryDocument_

queryDocument_ :: QuerySelector -> Effect (Array Element)
queryDocument_ querySelector =
  window
    >>= Window.document
    >>= toDocument
    >>> Document.toParentNode
    >>> pure
    >>= querySelectorAll querySelector
    >>= nodesToElements

nodesToElements :: NodeList -> Effect (Array Element)
nodesToElements = NodeList.toArray >=> map Element.fromNode >>> Array.catMaybes >>> pure

getSwapId :: Array TestMap -> String -> Maybe String
getSwapId testMaps variantId = Array.find (_.targetId >>> (==) variantId) testMaps <#> _.swapId

-- instance domActionTest :: DomInteraction ?identity where
--   getAttribute = ?get
--   setAttribute = ?set

-- class SiteCapabilities a where
--   swapProductPrices :: NonEmptyArray Element -> a
--   swapCheckoutOptions :: NonEmptyArray Element -> a
--   collectPriceElements :: a -> Effect (Array Element)

-- data SC a --   = SwapProductPrices (NonEmptyArray Element -> a)
--   | SwapCheckoutOptions (NonEmptyArray Element -> a)
--   | CollectPriceElements (a -> Effect (Array Element))
