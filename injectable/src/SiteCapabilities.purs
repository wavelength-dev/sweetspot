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
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import SweetSpot.Data.Config (idClass) as Config
import SweetSpot.Data.Domain (TestMapsMap)
import SweetSpot.SiteCapabilities.Dom (awaitDomReady, queryDocument_, removeClass) as SiteDom
import SweetSpot.SiteCapabilities.PriceControl (setControlledPrice) as SitePriceControl
import SweetSpot.SiteCapabilities.Url (getPathname, getUrlParam_, replacePathname) as SiteUrl
import Web.DOM (Element)
import Web.DOM.Element (className, getAttribute, setAttribute, setClassName) as Element
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML (HTMLElement)

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

setControlledPrice :: TestMapsMap -> Element -> Effect Unit
setControlledPrice = SitePriceControl.setControlledPrice
