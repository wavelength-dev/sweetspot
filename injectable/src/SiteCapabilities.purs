module SweetSpot.SiteCapabilities where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, makeAff, nonCanceler)
import SweetSpot.Data.Config (DryRunMode(..))
import SweetSpot.Data.Config (dryRunMode, idClass) as Config
import SweetSpot.Data.Domain (Sku(..), TestMapsMap)
import SweetSpot.Intl (formatPrice) as Intl
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
import Web.HTML.History (DocumentTitle(..), replaceState, state, URL(..))
import Web.HTML.Location (pathname)
import Web.HTML.Window as Window

class
  Monad m <= DomAction m where
  getAttribute :: String -> Element -> m (Maybe String)
  setAttribute :: String -> String -> Element -> m Unit
  queryDocument :: QuerySelector -> m (Array Element)

instance domActionEffect :: DomAction Effect where
  getAttribute = Element.getAttribute
  setAttribute = Element.setAttribute
  queryDocument = queryDocument_

priceElementSelector :: QuerySelector
priceElementSelector = QuerySelector $ "[class*=" <> Config.idClass <> "]"

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

data Site
  = Longvadon
  | LibertyPrice
  | Unknown

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

getIdFromPriceElement :: Element -> Effect (Maybe Sku)
getIdFromPriceElement el = do
  classNames <- (String.split $ String.Pattern " ") <$> Element.className el
  pure $ findSweetSpotTag classNames >>= getSkuFromTag <#> Sku
  where
  findSweetSpotTag :: Array String -> Maybe String
  findSweetSpotTag = Array.find (String.contains (String.Pattern Config.idClass))

  getSkuFromTag :: String -> Maybe String
  getSkuFromTag tag = Array.last $ String.split (String.Pattern "--") tag

setPrice :: Number -> Element -> Effect Unit
setPrice price el = do
  formattedPrice <- Intl.formatPrice price
  setTextContent formattedPrice (Element.toNode el)

getPathname :: Effect String
getPathname = window >>= Window.location >>= pathname

replacePathname :: String -> Effect Unit
replacePathname url = do
  h <- window >>= Window.history
  state h >>= \st -> replaceState st (DocumentTitle "") (URL url) h

queryDocument_ :: QuerySelector -> Effect (Array Element)
queryDocument_ querySelector =
  window
    >>= Window.document
    >>= HTMLDocument.toDocument
    >>> Document.toParentNode
    >>> pure
    >>= ParentNode.querySelectorAll querySelector
    >>= nodesToElements

nodesToElements :: NodeList -> Effect (Array Element)
nodesToElements = NodeList.toArray >=> map Element.fromNode >>> Array.catMaybes >>> pure

applyPriceVariation :: TestMapsMap -> Element -> Effect Unit
applyPriceVariation testMaps el = do
  mElementSku <- getIdFromPriceElement el
  let
    mTestMap = mElementSku >>= (\sku -> Array.find (_.sku >>> (==) sku) testMaps)
  case mTestMap, Config.dryRunMode of
    (Just testMap), DryRun -> do
      formattedPrice <- Intl.formatPrice testMap.swapPrice
      Element.setAttribute "data-ssdr__price" formattedPrice el
    (Just testMap), Live -> setPrice testMap.swapPrice el
    Nothing, _ -> pure unit
