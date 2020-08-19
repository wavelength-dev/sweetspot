module Fulcrum.Checkout where

import Prelude
import Control.Monad.Except (runExceptT, throwError)
import Data.Array (head) as Array
import Data.Either (Either(..))
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Datadog (logError) as Logger
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception as Unsafe
import Effect.Exception.Unsafe (unsafeThrow)
import Fulcrum.Config as Config
import Fulcrum.Data (TestMapByVariant, VariantId(..))
import Fulcrum.Site as Site
import Web.DOM (Element)
import Web.DOM.Document (createElement) as Document
import Web.DOM.Element (getAttribute, setAttribute, toNode) as Element
import Web.DOM.Element (id)
import Web.DOM.Node as Node
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML (window) as HTML
import Web.HTML.HTMLDocument (toDocument) as HTMLDocument
import Web.HTML.HTMLSelectElement as HTMLSelectElement
import Web.HTML.Window (document, localStorage) as Window
import Web.Storage.Storage (getItem) as Storage

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
  max-width: 217px;
  """

highlightCheckout :: Effect Unit
highlightCheckout = do
  window <- HTML.window
  document <- Window.document window >>= HTMLDocument.toDocument >>> pure
  matchingElements <- Site.queryDocument (QuerySelector "[data-product-form]")
  case Array.head matchingElements of
    Nothing -> mempty
    Just formElement -> do
      cartToken <- HTML.window >>= Window.localStorage >>= Storage.getItem Config.tokenStashKey
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
  isDebugging <- Site.getIsDebugging
  isDryRun <- Site.getIsDryRun
  when isDebugging highlightCheckout
  optionElements <- Site.queryDocument (QuerySelector "option.sweetspot__option")
  unless isDryRun do
    traverse_ (setCheckoutVariantId testMap) optionElements
  when (isDryRun && isDebugging) do
    traverse_ (setCheckoutVariantId testMap) optionElements

setCheckout :: TestMapByVariant -> Effect Unit
setCheckout testMap = do
  isDebugging <- Site.getIsDebugging
  isDryRun <- Site.getIsDryRun
  mEls <- Site.queryDocument (QuerySelector "#ProductSelect")
  let
    el = case Array.head mEls of
      Nothing -> unsafeThrow "no product select found"
      Just firstEl -> firstEl

    selectEl = case HTMLSelectElement.fromElement el of
      Nothing -> unsafeThrow "product select is not a select element"
      Just narrowEl -> narrowEl
  rawTargetId <- HTMLSelectElement.value selectEl <#> VariantId
  case Map.lookup rawTargetId testMap of
    -- the id may be not a sweetspot id, already swapped or unknown
    Nothing -> mempty
    Just test -> do
      unless isDryRun do
        HTMLSelectElement.setValue test.swapId selectEl
      when (isDryRun && isDebugging) do
        HTMLSelectElement.setValue test.swapId selectEl

-- on established titles the #ProductSelect has its value updated through JavaScript
-- we react
observeCheckout :: TestMapByVariant -> Effect Unit
observeCheckout testMap =
  Site.queryDocument (QuerySelector "[data-product-form]")
    >>= Array.head
    >>> case _ of
        Nothing -> Unsafe.throw "no product form found"
        Just el -> Site.onElementsMutation { subtree: true, childList: true } (\_ -> setCheckout testMap) [ el ]
