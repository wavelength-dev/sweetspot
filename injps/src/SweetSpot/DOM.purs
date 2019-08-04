module SweetSpot.DOM where

import Prelude
import Data.Array (catMaybes)
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Number.Format (toString)
import Data.String as S
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Global (readFloat)
import SweetSpot.Data.Api (UserBucket)
import SweetSpot.Data.Constant (DryRunMode(..), dryRunMode, idClass)
import SweetSpot.Data.Product (Sku(..))
import SweetSpot.Intl (formatNumber, numberFormat)
import Web.DOM.DOMTokenList as DTL
import Web.DOM.Document (getElementsByTagName, toParentNode)
import Web.DOM.Element as E
import Web.DOM.HTMLCollection as HC
import Web.DOM.Internal.Types (Element, Node)
import Web.DOM.Node (setTextContent)
import Web.DOM.NodeList as NL
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (HTMLElement, window)
import Web.HTML.Event.EventTypes (domcontentloaded)
import Web.HTML.HTMLDocument (toDocument, toEventTarget)
import Web.HTML.HTMLElement (classList)
import Web.HTML.History (DocumentTitle(..), replaceState, state, URL(..))
import Web.HTML.Location (pathname)
import Web.HTML.Window (document, location, history)

getDOMReady :: Aff Unit
getDOMReady =
  makeAff \cb -> do
    listener <- eventListener (\_ -> cb (Right unit))
    doc <- window >>= document
    addEventListener domcontentloaded listener false (toEventTarget doc)
    pure nonCanceler

collectPriceEls :: Effect (Array Element)
collectPriceEls = do
  htmlDoc <- window >>= document
  let
    docNode = toParentNode <<< toDocument $ htmlDoc
  checkoutOptionNodes <- querySelectorAll (QuerySelector ("[class*=" <> idClass <> "]")) docNode
  nodesArray <- NL.toArray checkoutOptionNodes
  pure $ catMaybes (map E.fromNode nodesArray)

collectCheckoutOptions :: NonEmptyArray Number -> Effect (Array Element)
collectCheckoutOptions variantIds = do
  doc <- window >>= document
  elements <- getElementsByTagName "option" (toDocument doc) >>= HC.toArray
  -- return any element with a value attribute value equal to one of variantIds
  A.filterA getIsKnownVariantOption elements
  where
  getIsKnownVariantOption :: Element -> Effect Boolean
  getIsKnownVariantOption el = do
    optionId <- E.getAttribute "value" el
    pure $ maybe false (\id -> A.elem (readFloat id) variantIds) optionId

collectLongvadonCheckoutOptions :: NonEmptyArray Number -> Effect (Array Element)
collectLongvadonCheckoutOptions variantIds = do
  htmlDoc <- window >>= document
  let
    docNode = toParentNode <<< toDocument $ htmlDoc
  checkoutOptionNodes <- querySelectorAll (QuerySelector "[data-varid]") docNode
  nodesArray <- NL.toArray checkoutOptionNodes
  -- We expect these elements to be divs, div nodes can be converted to Element, so we ignore nodes which can not be converted to elements, as they shouldn't exist.
  let
    elements = catMaybes (map E.fromNode nodesArray)
  A.filterA getIsKnownVariantOption elements
  where
  getIsKnownVariantOption :: Element -> Effect Boolean
  getIsKnownVariantOption el = do
    dataVarid <- E.getAttribute "data-varid" el
    pure $ maybe false (\id -> A.elem (readFloat id) variantIds) dataVarid

getMatchingUserBucket :: NonEmptyArray UserBucket -> String -> Maybe UserBucket
getMatchingUserBucket buckets id =
  A.find
    ((==) (readFloat id) <<< _._ubOriginalSvid)
    buckets

getOptionVariantId :: NonEmptyArray UserBucket -> String -> Element -> Effect (Maybe String)
getOptionVariantId buckets attribute el = do
  attrValue <- E.getAttribute attribute el
  pure $ attrValue >>= getMatchingUserBucket buckets # map (toString <<<  _._ubTestSvid)

swapLongvadonCheckoutVariantId :: NonEmptyArray UserBucket -> Array Element -> Effect Unit
swapLongvadonCheckoutVariantId buckets elements = traverse_ swapCheckoutIds elements
  where
  swapCheckoutIds :: Element -> Effect Unit
  swapCheckoutIds el = do
    mVariantId <- getOptionVariantId buckets "data-varid" el
    case mVariantId, dryRunMode of
      Nothing, _ -> pure unit
      (Just variantId), DryRun -> E.setAttribute "ssdr__data-varid" variantId el
      (Just variantId), Live -> E.setAttribute "data-varid" variantId el

swapLibertyPriceCheckoutVariantId :: NonEmptyArray UserBucket -> Array Element -> Effect Unit
swapLibertyPriceCheckoutVariantId buckets = traverse_ swapCheckoutIds
  where
  swapCheckoutIds :: Element -> Effect Unit
  swapCheckoutIds el = do
    mVariantId <- getOptionVariantId buckets "value" el
    case mVariantId, dryRunMode of
      Nothing, _ -> pure unit
      (Just variantId), DryRun -> E.setAttribute "ssdr__value" variantId el
      (Just variantId), Live -> E.setAttribute "value" variantId el

removeClass :: String -> HTMLElement -> Effect Unit
removeClass className = classList >=> remove' className
  where
  remove' = flip DTL.remove

addClass :: String -> Element -> Effect Unit
addClass className el = do
  current <- E.className el
  E.setClassName (current <> " " <> className) el

getIdFromPriceElement :: Element -> Effect (Maybe Sku)
getIdFromPriceElement el = do
  classNames <- (S.split $ S.Pattern " ") <$> E.className el
  let
    match = A.find (S.contains (S.Pattern idClass)) classNames

    sku = A.last =<< (S.split $ S.Pattern "--") <$> match
  pure $ Sku <$> sku

setNodePrice :: Number -> Node -> Effect Unit
setNodePrice price node = do
  nf <- numberFormat
  formattedPrice <- formatNumber price nf
  setTextContent formattedPrice node

setPrice :: Number -> Element -> Effect Unit
setPrice price el = setNodePrice price (E.toNode el)

getPathname :: Effect String
getPathname = window >>= location >>= pathname

replacePathname :: String -> Effect Unit
replacePathname url = do
  h <- window >>= history
  state h >>= \st -> replaceState st (DocumentTitle "") (URL url) h
