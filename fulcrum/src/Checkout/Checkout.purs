module Fulcrum.Checkout where

import Prelude
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Array (head) as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map (findMin, fromFoldable, lookup) as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Fulcrum.Checkout.Highlight (highlightCheckout)
import Fulcrum.Data (TestMapByVariant, VariantId(..))
import Fulcrum.EstablishedTitles (isCurrentSite) as EstablishedTitles
import Fulcrum.Logger (LogLevel(..))
import Fulcrum.Logger (log, logWithContext) as Logger
import Fulcrum.Site (getIsDebugging, getIsDryRun, getUrlParam, onElementsMutation, queryDocument, readHostname) as Site
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)
import Web.DOM.Element (getAttribute, setAttribute, toNode) as Element
import Web.DOM.Node as Node
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML (HTMLOptionElement)
import Web.HTML.HTMLOptionElement (setText, text) as HTMLOptionElement
import Web.HTML.HTMLSelectElement (fromElement, setValue, value) as HTMLSelectElement

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
        Left err -> Logger.logWithContext Error "failed to set checkout variant id" err
        _ -> mempty

setTestCheckout :: TestMapByVariant -> Effect Unit
setTestCheckout testMap = do
  isDryRun <- Site.getIsDryRun
  -- overwriting the text in the available options
  isLibertyPrice <- Site.readHostname <#> (==) "libertyprice.myshopify.com"
  isEstablishedTitles <- EstablishedTitles.isCurrentSite
  -- logic for debug runs
  isDebugging <- Site.getIsDebugging
  when isDebugging highlightCheckout
  optionElements <- Site.queryDocument (QuerySelector "option.sweetspot__option")
  let
    isTestPrice = case Map.findMin testMap of
      Nothing -> false
      Just { value: { variantId, swapId } } -> variantId /= swapId
  let
    execute = do
      traverse_ (setCheckoutVariantId testMap) optionElements
      when ((isEstablishedTitles || isLibertyPrice) && isTestPrice) setOptionTexts
  unless isDryRun execute
  when (isDryRun && isDebugging) execute

-- The select `value` attribute is empty sometimes, probably because
-- shopify tries to set it too and as they don't recognize the options
-- anymore the set value fails and empties the value. So we can't use
-- it to determine the value selected anymore.
-- Instead we read the variant from the URL, better would be to
-- read the selected options and figure out the right variant
-- ourselves.
onSelectVariant :: TestMapByVariant -> Effect Unit
onSelectVariant testMap = do
  isDebugging <- Site.getIsDebugging
  isDryRun <- Site.getIsDryRun
  mEls <- Site.queryDocument (QuerySelector "#ProductSelect")
  mTargetId <- Site.getUrlParam "variant"
  eSuccess <-
    runExceptT do
      el <- case Array.head mEls of
        Nothing -> throwError "no product select found"
        Just firstEl -> pure firstEl
      selectEl <- case HTMLSelectElement.fromElement el of
        Nothing -> throwError "product select is not a select element"
        Just narrowEl -> pure narrowEl
      rawTargetId <- case mTargetId of
        Nothing -> throwError "failed to read variant from URL"
        Just targetId -> VariantId targetId # pure
      case Map.lookup rawTargetId testMap of
        -- The id may not be a sweetspot id, already swapped or unknown
        -- Do nothing.
        Nothing -> pure unit :: ExceptT String Effect Unit
        Just test ->
          liftEffect do
            let
              execute = do
                -- if the value is there already we don't set it again
                current <- HTMLSelectElement.value selectEl
                unless (current == test.swapId) do
                  HTMLSelectElement.setValue test.swapId selectEl
            unless isDryRun execute
            when (isDryRun && isDebugging) execute
  case eSuccess of
    Left err -> Logger.logWithContext Error "failed to set checkout on select" err
    Right _ -> mempty

-- on established titles the #ProductSelect has its value updated through JavaScript
-- we react
registerOnSelectVariant :: TestMapByVariant -> Effect Unit
registerOnSelectVariant testMap =
  Site.queryDocument (QuerySelector "[data-product-form]")
    >>= Array.head
    >>> case _ of
        Nothing -> throw "no product form found"
        Just el ->
          Site.onElementsMutation
            { subtree: true, childList: true }
            (\_ -> onSelectVariant testMap)
            [ el ]

-- Maps a form variant option text to another for Established Titles
type OptionTextMap
  = Map String String

optionTextMap :: OptionTextMap
optionTextMap =
  Map.fromFoldable
    -- [ Tuple "1 Sq Ft" "1 Sq Ft"
    -- , Tuple "5 Sq Ft (+$160)" "5 Sq Ft (+$150)"
    -- , Tuple "10 Sq Ft (+$300)" "10 Sq Ft (+$290)"
    -- , Tuple "Digital Only" "Digital Only"
    [ Tuple "Add Print +$30" "Add Print +$20"
    -- , Tuple "No" "No"
    -- , Tuple "Yes +$59" "Yes +$59"
    -- liberty price test option
    , Tuple "custom" "custom +$5"
    ]

-- Maps a form variant option label text to another for Established Titles
type LabelTextMap
  = Map String String

labelTextMap :: LabelTextMap
labelTextMap =
  Map.fromFoldable
    [ Tuple "Add a Printed Certificate for $30?" "Add a Printed Certificate for $20?"
    ]

setOptionTexts :: Effect Unit
setOptionTexts =
  -- Our query selector is for option elements, we can safely
  -- assume they are.
  Site.queryDocument (QuerySelector ".product-form__input > option")
    <#> map unsafeToOption
    >>= traverse_ \optionElement -> do
        mTargetText <- HTMLOptionElement.text optionElement
        case lookup optionTextMap mTargetText of
          -- TODO: use exceptT with logging
          Nothing -> Logger.log Warn "unrecognized variant option"
          Just swapText -> HTMLOptionElement.setText swapText optionElement
  where
  lookup = flip Map.lookup

setLabelTexts :: Effect Unit
setLabelTexts =
  Site.queryDocument (QuerySelector "label")
    <#> map Element.toNode
    >>= traverse_ \node -> do
        text <- Node.textContent node
        case lookup labelTextMap text of
          -- don't recoginze the element, do nothing.
          Nothing -> pure unit
          Just swapText -> Node.setTextContent swapText node
  where
  lookup = flip Map.lookup

unsafeToOption :: Element -> HTMLOptionElement
unsafeToOption = unsafeCoerce
