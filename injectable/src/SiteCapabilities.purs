module SweetSpot.SiteCapabilities
  ( class BrowserAction
  , awaitDomReady
  , getAttribute
  , getPathname
  , getUrlParam
  , priceElementSelector
  , queryDocument
  , replacePathname
  , revealPrice
  , setAttribute
  , setControlledPrice
  ) where

import Prelude

import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import SweetSpot.Data.Config (hiddenPriceId, idClass) as Config
import SweetSpot.Data.Domain (Sku, TestMap)
import SweetSpot.SiteCapabilities.Dom (awaitDomReady, queryDocument_, removeClass) as SiteDom
import SweetSpot.SiteCapabilities.PriceControl (setControlledPrice) as SitePriceControl
import SweetSpot.SiteCapabilities.Url (getPathname, getUrlParam_, replacePathname) as SiteUrl
import Web.DOM (Element)
import Web.DOM.Element (className, getAttribute, setAttribute, setClassName) as Element
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML (HTMLElement)
import Web.HTML.HTMLElement (fromElement) as HTMLElement

class
  Monad m <= BrowserAction m where
  getAttribute :: String -> Element -> m (Maybe String)
  setAttribute :: String -> String -> Element -> m Unit
  queryDocument :: QuerySelector -> m (Array Element)
  getUrlParam :: String -> m (Maybe String)

instance browserActionEffect :: BrowserAction Effect where
  getAttribute = Element.getAttribute
  setAttribute = Element.setAttribute
  queryDocument = SiteDom.queryDocument_
  getUrlParam = SiteUrl.getUrlParam_

priceElementSelector :: QuerySelector
priceElementSelector = QuerySelector $ "[class*=" <> Config.idClass <> "]"

data Site
  = Longvadon
  | LibertyPrice
  | Unknown

awaitDomReady :: Aff Unit
awaitDomReady = SiteDom.awaitDomReady

removeClass :: String -> HTMLElement -> Effect Unit
removeClass = SiteDom.removeClass

-- TODO: replace with web-dom
addClass :: String -> Element -> Effect Unit
addClass className el = do
  current <- Element.className el
  Element.setClassName (current <> " " <> className) el

getPathname :: Effect String
getPathname = SiteUrl.getPathname

replacePathname :: String -> Effect Unit
replacePathname = SiteUrl.replacePathname

setControlledPrice :: Map Sku TestMap -> Element -> Effect Unit
setControlledPrice = SitePriceControl.setControlledPrice

revealPrice :: Element -> Effect Unit
revealPrice element = do
  case HTMLElement.fromElement element of
    Nothing -> pure unit
    Just htmlElement -> removeClass Config.hiddenPriceId htmlElement
