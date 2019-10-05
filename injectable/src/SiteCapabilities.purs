module SweetSpot.SiteCapabilities
  ( class BrowserAction
  , awaitDomReady
  , getAttribute
  , getPathname
  , getUrlParam
  , priceElementSelector
  , queryDocument
  , removeClass
  , replacePathname
  , setAttribute
  , setControlledPrice
  ) where

import Prelude
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (oneOf)
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, makeAff, nonCanceler)
import SweetSpot.Data.Config (DryRunMode(..))
import SweetSpot.Data.Config (dryRunMode, idClass) as Config
import SweetSpot.Data.Domain (Sku(..), TestMapsMap)
import SweetSpot.Intl (formatPrice) as Intl
import SweetSpot.QueryString (QueryParam(..))
import SweetSpot.QueryString (parseQueryString) as QueryString
import Web.DOM (Element, NodeList)
import Web.DOM.DOMTokenList as DTL
import Web.DOM.Document (toParentNode) as Document
import Web.DOM.Element as Element
import Web.DOM.Node (setTextContent)
import Web.DOM.NodeList (toArray) as NodeList
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.ParentNode (querySelectorAll) as ParentNode
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (HTMLElement, window)
import Web.HTML.Event.EventTypes as ET
import Web.HTML.HTMLDocument (readyState, toDocument) as HTMLDocument
import Web.HTML.HTMLDocument.ReadyState (ReadyState(..))
import Web.HTML.HTMLElement (classList)
import Web.HTML.History (DocumentTitle(..), URL(..))
import Web.HTML.History (replaceState, state) as History
import Web.HTML.Location (pathname, search) as Location
import Web.HTML.Window as Window

class
  Monad m <= BrowserAction m where
  getAttribute :: String -> Element -> m (Maybe String)
  setAttribute :: String -> String -> Element -> m Unit
  queryDocument :: QuerySelector -> m (Array Element)
  getUrlParam :: String -> m (Maybe String)

instance browserActionEffect :: BrowserAction Effect where
  getAttribute = Element.getAttribute
  setAttribute = Element.setAttribute
  queryDocument = queryDocument_
  getUrlParam = getUrlParam_

priceElementSelector :: QuerySelector
priceElementSelector = QuerySelector $ "[class*=" <> Config.idClass <> "]"

data Site
  = Longvadon
  | LibertyPrice
  | Unknown

--
-- TODO: SweetSpot.SiteCapabilities.DOM
--
awaitDomReady :: Aff Unit
awaitDomReady =
  makeAff \callback -> do
    rs <- HTMLDocument.readyState =<< Window.document =<< window
    case rs of
      Loading -> do
        et <- Window.toEventTarget <$> window
        listener <- eventListener (\_ -> callback (Right unit))
        addEventListener ET.domcontentloaded listener false et
        pure $ effectCanceler (removeEventListener ET.domcontentloaded listener false et)
      _ -> do
        callback (Right unit)
        pure nonCanceler

removeClass :: String -> HTMLElement -> Effect Unit
removeClass className = classList >=> (flip DTL.remove) className

addClass :: String -> Element -> Effect Unit
addClass className el = do
  current <- Element.className el
  Element.setClassName (current <> " " <> className) el

nodesToElements :: NodeList -> Effect (Array Element)
nodesToElements = NodeList.toArray >=> map Element.fromNode >>> Array.catMaybes >>> pure

queryDocument_ :: QuerySelector -> Effect (Array Element)
queryDocument_ querySelector =
  window
    >>= Window.document
    >>= HTMLDocument.toDocument
    >>> Document.toParentNode
    >>> pure
    >>= ParentNode.querySelectorAll querySelector
    >>= nodesToElements

--
-- TODO: SweetSpot.SiteCapabilities.URL
--
getPathname :: Effect String
getPathname = window >>= Window.location >>= Location.pathname

replacePathname :: String -> Effect Unit
replacePathname url = do
  history <- window >>= Window.history
  History.state history >>= \historyState -> History.replaceState historyState (DocumentTitle "") (URL url) history

getUrlParam_ :: String -> Effect (Maybe String)
getUrlParam_ targetKey = do
  queryString <- window >>= Window.location >>= Location.search
  queryString
    # QueryString.parseQueryString
    >>> map matchQueryParam
    >>> oneOf
    >>> pure
  where
  matchQueryParam :: Either String QueryParam -> Maybe String
  matchQueryParam eQueryParam = case eQueryParam of
    Right (QueryParam key (Just value)) ->
      if key == targetKey then
        Just value
      else
        Nothing
    _ -> Nothing

-- TODO: SweetSpot.SiteCapabilities.PriceControl
-- TODO: make this an Int
type Price
  = Number

setControlledPrice :: TestMapsMap -> Element -> Effect Unit
setControlledPrice testMaps el = do
  mElementSku <- getIdFromPriceElement el
  let
    mTestMap = mElementSku >>= (\sku -> Array.find (_.sku >>> (==) sku) testMaps)
  case mTestMap, Config.dryRunMode of
    (Just testMap), DryRun -> do
      formattedPrice <- Intl.formatPrice testMap.swapPrice
      Element.setAttribute "data-ssdr__price" formattedPrice el
    (Just testMap), Live -> do
      formattedPrice <- Intl.formatPrice testMap.swapPrice
      setTextContent formattedPrice (Element.toNode el)
    Nothing, _ -> pure unit

getIdFromPriceElement :: Element -> Effect (Maybe Sku)
getIdFromPriceElement el = do
  classNames <- (String.split $ String.Pattern " ") <$> Element.className el
  pure $ findSweetSpotTag classNames >>= getSkuFromTag <#> Sku
  where
  findSweetSpotTag :: Array String -> Maybe String
  findSweetSpotTag = Array.find (String.contains (String.Pattern Config.idClass))

  getSkuFromTag :: String -> Maybe String
  getSkuFromTag tag = Array.last $ String.split (String.Pattern "--") tag
