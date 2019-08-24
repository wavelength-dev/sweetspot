module SweetSpot.DOM where

import Prelude

import Data.Array (catMaybes)
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Number.Format (toString)
import Data.String as S
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Global (readFloat)
import SweetSpot.Data.Api (UserBucket)
import SweetSpot.Data.Config (idClass)
import SweetSpot.Data.Product (Sku(..))
import SweetSpot.Intl (formatNumber, numberFormat)
import Web.DOM (Element, Node)
import Web.DOM.DOMTokenList as DTL
import Web.DOM.Document as Doc
import Web.DOM.Element as E
import Web.DOM.HTMLCollection as HC
import Web.DOM.Node (setTextContent, textContent)
import Web.DOM.NodeList as NL
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (HTMLElement, window)
import Web.HTML.Event.EventTypes (domcontentloaded)
import Web.HTML.HTMLDocument (toDocument, toEventTarget)
import Web.HTML.HTMLElement (classList)
import Web.HTML.History (DocumentTitle(..), replaceState, state, URL(..))
import Web.HTML.Location (hostname, pathname)
import Web.HTML.Window as Win

data Site
  = Longvadon
  | LibertyPrice
  | Unknown

getSiteId :: Effect (Maybe Site)
getSiteId = do
  hostUrl <- window >>= Win.location >>= hostname
  doc <- window >>= Win.document >>= toDocument >>> Doc.toParentNode >>> pure
  mEl <- querySelector (QuerySelector "#sweetspot__site-id") doc
  let
    textToSite text = case text of
      "longvadon" -> Just Longvadon
      "libertyprice" -> Just LibertyPrice
      _ -> Nothing
  maybe
    (pure $ Nothing)
    (\el -> E.toNode el # textContent >>= textToSite >>> pure)
    mEl

getDOMReady :: Aff Unit
getDOMReady =
  makeAff \cb -> do
    listener <- eventListener (\_ -> cb (Right unit))
    doc <- window >>= Win.document
    addEventListener domcontentloaded listener false (toEventTarget doc)
    pure nonCanceler

collectPriceEls :: Effect (Array Element)
collectPriceEls = do
  docNode <- window >>= Win.document >>= toDocument >>> Doc.toParentNode >>> pure
  checkoutOptionNodes <-
    querySelectorAll
      (QuerySelector ("[class*=" <> idClass <> "]"))
      docNode
  nodesArray <- NL.toArray checkoutOptionNodes
  pure $ catMaybes (map E.fromNode nodesArray)

collectCheckoutOptions :: NonEmptyArray Number -> Effect (Array Element)
collectCheckoutOptions variantIds = do
  doc <- window >>= Win.document
  elements <- Doc.getElementsByTagName "option" (toDocument doc) >>= HC.toArray
  -- return any element with a value attribute value equal to one of variantIds
  A.filterA getIsKnownVariantOption elements
  where
  getIsKnownVariantOption :: Element -> Effect Boolean
  getIsKnownVariantOption el = do
    optionId <- E.getAttribute "value" el
    pure $ maybe false (\id -> A.elem (readFloat id) variantIds) optionId

getMatchingUserBucket :: NonEmptyArray UserBucket -> String -> Maybe UserBucket
getMatchingUserBucket buckets id =
  A.find
    ((==) (readFloat id) <<< _._ubOriginalSvid)
    buckets

getOptionVariantId :: NonEmptyArray UserBucket -> String -> Element -> Effect (Maybe String)
getOptionVariantId buckets attribute el = do
  attrValue <- E.getAttribute attribute el
  pure $ attrValue >>= getMatchingUserBucket buckets # map (toString <<< _._ubTestSvid)

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
getPathname = window >>= Win.location >>= pathname

replacePathname :: String -> Effect Unit
replacePathname url = do
  h <- window >>= Win.history
  state h >>= \st -> replaceState st (DocumentTitle "") (URL url) h
