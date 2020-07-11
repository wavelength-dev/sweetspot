module Fulcrum.Checkout where

import Prelude
import Control.Monad.Except (runExceptT, throwError)
import Data.Array (catMaybes, head) as Array
import Data.Either (Either(..))
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Datadog (logError) as Logger
import Effect (Effect)
import Effect.Class (liftEffect)
import Fulcrum.Data (TestMapByVariant, VariantId(..))
import Fulcrum.Logger (getIsDebugging)
import Web.DOM (Element)
import Web.DOM.Document (createElement, toParentNode) as Document
import Web.DOM.Element (fromNode, getAttribute, setAttribute, toNode) as Element
import Web.DOM.Node as Node
import Web.DOM.NodeList (toArray) as NodeList
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.ParentNode (querySelectorAll) as ParentNode
import Web.HTML (window) as HTML
import Web.HTML.HTMLDocument (toDocument) as HTMLDocument
import Web.HTML.Window (document) as Window

-- Supported checkout: Shopify default product-form
-- We target option.sweetspot__option and swap out the value attribute.
--
-- <form
--   method="post"
--   action="/cart/add"
--   id="product_form_1857560051776"
--   accept-charset="UTF-8"
--   class="product-form product-form-product-template product-form--payment-button-no-variants"
--   enctype="multipart/form-data"
--   novalidate="novalidate"
--   style="border-style: solid; border-color: #5c6ac4;"
-- >
--   <input type="hidden" name="form_type" value="product" /><input
--     type="hidden"
--     name="utf8"
--     value="✓"
--   />
--
--   <select
--     name="id"
--     id="ProductSelect-product-template"
--     class="product-form__variants no-js"
--   >
--     <option selected="selected" value="24783368781888" class="sweetspot__option">
--       Default Title
--     </option>
--   </select>
--
--   <div
--     class="product-form__item product-form__item--submit product-form__item--payment-button product-form__item--no-variants"
--   >
--      ..
--   </div>
-- </form>
--
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
setCheckoutVariantId testMap element =
  runExceptT do
    mValue <- Element.getAttribute "value" element # liftEffect
    targetVariantId <- case mValue of
      Nothing -> throwError "checkout option missing value attribute"
      Just targetVariantId -> pure targetVariantId
    case Map.lookup (VariantId targetVariantId) testMap of
      -- variant is not under test
      Nothing -> pure unit
      Just test -> Element.setAttribute "value" test.swapId element # liftEffect
    >>= case _ of
        Left err -> Logger.logError err
        _ -> mempty

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
  """

highlightCheckout :: Effect Unit
highlightCheckout = do
  document <- HTML.window >>= Window.document >>= HTMLDocument.toDocument >>> pure
  matchingElements <- queryDocument (QuerySelector "form.product-form")
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

applyTestCheckout :: TestMapByVariant -> Effect Unit
applyTestCheckout testMap = do
  isDebugging <- getIsDebugging
  when isDebugging highlightCheckout
  optionElements <- queryDocument (QuerySelector "option.sweetspot__option")
  traverse_ (setCheckoutVariantId testMap) optionElements
  where
  -- We discard nodes that are not elements.
  nodesToElements = NodeList.toArray >=> map Element.fromNode >>> Array.catMaybes >>> pure
