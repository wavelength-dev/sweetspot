module Fulcrum.Checkout.Highlight where

import Prelude
import Data.Array (head) as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Fulcrum.Site (queryDocument) as Site
import Web.DOM.Document (createElement) as Document
import Web.DOM.Element (setAttribute, toNode) as Element
import Web.DOM.Node (appendChild, firstChild, insertBefore, setTextContent) as Node
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML (window) as HTML
import Web.HTML.HTMLDocument (toDocument) as HTMLDocument
import Web.HTML.Window (document) as Window

formHighlight :: String
formHighlight =
  """
  border-style: solid;
  border-color: #5c6ac4;
  """

-- label
labelHighlight :: String
labelHighlight =
  """
  background-color: white;
  color: #5c6ac4;
  padding: 0px 8px;
  margin: -2.9rem 0px auto 5px;
  max-width: 217px;
  """

highlightCheckout :: Effect Unit
highlightCheckout = do
  document <- HTML.window >>= Window.document <#> HTMLDocument.toDocument
  matchingElements <- Site.queryDocument (QuerySelector "[data-product-form]")
  case Array.head matchingElements of
    Nothing -> mempty
    Just formElement -> do
      labelElement <- Document.createElement "p" document
      let
        labelNode = Element.toNode labelElement

        formNode = Element.toNode formElement
      Element.setAttribute "style" labelHighlight labelElement
      Element.setAttribute "style" formHighlight formElement
      Node.setTextContent "sweetspot controlled checkout" labelNode
      mFirstChild <- Node.firstChild formNode
      case mFirstChild of
        Nothing -> Node.appendChild labelNode formNode *> mempty
        Just firstChild -> Node.insertBefore labelNode firstChild formNode *> mempty
