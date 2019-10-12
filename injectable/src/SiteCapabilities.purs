module SweetSpot.SiteCapabilities
  ( class BrowserAction
  , awaitDomReady
  , getAttribute
  , getPathname
  , getUrlParam
  , mkMutationObserver
  , observeForMutation
  , priceElementSelector
  , queryDocument
  , replacePathname
  , revealPrice
  , setAttribute
  , setControlledPrice
  , setControlledPriceM
  ) where

import Prelude

import Control.Monad.Reader (class MonadAsk, ReaderT)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Prim.Row (class Union)
import SweetSpot.Data.Config (hiddenPriceId, idClass) as Config
import SweetSpot.Data.Domain (TestContext)
import SweetSpot.SiteCapabilities.Dom (awaitDomReady, queryDocument_, removeClass) as SiteDom
import SweetSpot.SiteCapabilities.PriceControl (setControlledPrice, setControlledPriceM) as SitePriceControl
import SweetSpot.SiteCapabilities.Url (getPathname, getUrlParam_, replacePathname) as SiteUrl
import Web.DOM (Element, Node)
import Web.DOM.Element (className, getAttribute, setAttribute, setClassName) as Element
import Web.DOM.MutationObserver (MutationObserver, MutationObserverInitFields)
import Web.DOM.MutationObserver as MutationObserver
import Web.DOM.MutationRecord (MutationRecord)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML (HTMLElement)
import Web.HTML.HTMLElement (fromElement) as HTMLElement

class
  Monad m <= BrowserAction m where
  getAttribute :: String -> Element -> m (Maybe String)
  getUrlParam :: String -> m (Maybe String)
  mkMutationObserver :: (Array MutationRecord → MutationObserver → Effect Unit) → m MutationObserver
  observeForMutation :: ∀ r rx. Union r rx MutationObserverInitFields ⇒ Node → Record r → MutationObserver → m Unit
  queryDocument :: QuerySelector -> m (Array Element)
  setAttribute :: String -> String -> Element -> m Unit

instance browserActionEffect :: BrowserAction Effect where
  getAttribute = Element.getAttribute
  getUrlParam = SiteUrl.getUrlParam_
  mkMutationObserver = MutationObserver.mutationObserver
  observeForMutation = MutationObserver.observe
  queryDocument = SiteDom.queryDocument_
  setAttribute = Element.setAttribute

instance browserActionReaderT :: BrowserAction (ReaderT context Effect) where
  getAttribute name el = liftEffect $ Element.getAttribute name el
  getUrlParam url = liftEffect $ SiteUrl.getUrlParam_ url
  mkMutationObserver callback = liftEffect $ MutationObserver.mutationObserver callback
  observeForMutation node options mutationObserver = liftEffect $ MutationObserver.observe node options mutationObserver
  queryDocument querySelector = liftEffect $ SiteDom.queryDocument_ querySelector
  setAttribute name value el = liftEffect $ Element.setAttribute name value el

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

setControlledPrice :: TestContext -> Element -> Effect Unit
setControlledPrice = SitePriceControl.setControlledPrice

setControlledPriceM :: forall m. MonadEffect m => BrowserAction m => MonadAsk TestContext m => Element -> m Unit
setControlledPriceM = SitePriceControl.setControlledPriceM

revealPrice :: Element -> Effect Unit
revealPrice element = do
  case HTMLElement.fromElement element of
    Nothing -> pure unit
    Just htmlElement -> removeClass Config.hiddenPriceId htmlElement
