module SweetSpot.DOM where

import Prelude
import Data.Array (catMaybes, fold, head)
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Number.Format (toString)
import Data.String as S
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Global (readFloat)
import SweetSpot.Data.Api (UserBucket(..))
import SweetSpot.Data.Constant (hiddenPriceId, idClassPattern)
import SweetSpot.Intl (formatNumber, numberFormat)
import Web.DOM.DOMTokenList as DTL
import Web.DOM.Document (getElementsByClassName, getElementsByTagName, toParentNode)
import Web.DOM.Element as E
import Web.DOM.HTMLCollection as HC
import Web.DOM.Internal.Types (Element, Node)
import Web.DOM.Node (setTextContent)
import Web.DOM.NodeList as NL
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (domcontentloaded)
import Web.HTML.HTMLDocument (toDocument, toEventTarget)
import Web.HTML.HTMLElement (classList, fromElement)
import Web.HTML.Window (document)

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
  priceNodes <- querySelectorAll (QuerySelector "[class*=supple__price]") docNode
  nodesArray <- NL.toArray priceNodes
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
    pure
      $ case optionId of
          Nothing -> false
          Just id -> A.elem (readFloat id) variantIds

swapCheckoutVariantId :: NonEmptyArray UserBucket -> Array Element -> Effect Unit
swapCheckoutVariantId userBuckets elements = traverse_ swapCheckoutIds elements
  where
  swapCheckoutIds el =
    getOptionVariantId el
      >>= ( \variantId -> case variantId of
            Nothing -> pure unit
            Just vId -> (E.setAttribute "value" (toString vId) el)
        )

  getMatchingUserBucket :: String -> Maybe UserBucket
  getMatchingUserBucket id = A.find (\(UserBucket userBucket) -> userBucket._ubOriginalSvid == (readFloat id)) userBuckets

  getOptionVariantId :: Element -> Effect (Maybe Number)
  getOptionVariantId el = do
    attrValue <- E.getAttribute "value" el
    pure $ attrValue >>= getMatchingUserBucket # map (\(UserBucket ub) -> ub._ubTestSvid)

removeClass :: String -> Element -> Maybe (Effect Unit)
removeClass className el = (classList >=> remove' className) <$> fromElement el
  where
  remove' = flip DTL.remove

addClass :: String -> Element -> Effect Unit
addClass className el = do
  current <- E.className el
  E.setClassName (current <> " " <> className) el

getIdFromPriceElement :: Element -> Effect (Maybe String)
getIdFromPriceElement el = do
  classNames <- (S.split $ S.Pattern " ") <$> E.className el
  let
    match = A.find (S.contains idClassPattern) classNames

    sku = A.last =<< (S.split $ S.Pattern "--") <$> match
  pure sku

unhidePrice :: Effect (Maybe Unit)
unhidePrice = do
  elements <- collectPriceEls
  pure $ traverse_ removeHiddenPriceClass elements
  where
  removeHiddenPriceClass :: Element -> Maybe (Effect Unit)
  removeHiddenPriceClass = removeClass hiddenPriceId

setNodePrice :: Number -> Node -> Effect Unit
setNodePrice price node = do
  nf <- numberFormat
  formattedPrice <- formatNumber price nf
  setTextContent formattedPrice node

setPrice :: Number -> Element -> Effect Unit
setPrice price el = setNodePrice price (E.toNode el)
