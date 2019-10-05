module SweetSpot.Event (trackView) where

import Prelude

import Data.Argonaut as Ar
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe (fromMaybe) as Maybe
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Milkis (Response)
import SweetSpot.Api (sendEvent) as Api
import SweetSpot.AppM (getUserId)
import SweetSpot.Data.Config (productClass)
import SweetSpot.Data.Event (Page(..))
import SweetSpot.Data.Shopify (Product)
import SweetSpot.Event.PageDetection (getCurrentPage) as PageDetection
import SweetSpot.Log (LogLevel(..))
import SweetSpot.Log (log) as Log
import Web.DOM (Element)
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.HTMLCollection as HTMLCollection
import Web.DOM.Node as Node
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Location (href)
import Web.HTML.Window as Window

extractInjectedProductJSON :: Element -> Effect (Either String Product)
extractInjectedProductJSON el = do
  text <- Element.toNode >>> Node.textContent $ el
  text # (Ar.jsonParser >=> Ar.decodeJson) >>> pure

readInjectedProducts :: Effect (Either String (Array Product))
readInjectedProducts = do
  document <- HTML.window >>= Window.document >>= HTMLDocument.toDocument >>> pure
  productJsonElements <- Document.getElementsByClassName productClass document >>= HTMLCollection.toArray
  mProducts <- traverse extractInjectedProductJSON productJsonElements
  pure $ sequence mProducts

trackView :: Aff Response
trackView = do
  event <- liftEffect do
    userId <- getUserId
    page <- PageDetection.getCurrentPage >>= Maybe.fromMaybe Unknown >>> pure
    pageUrl <- HTML.window >>= Window.location >>= href
    eProducts <- readInjectedProducts
    productIds <- case eProducts of
      Left msg -> Log.log Warn msg *> pure Nothing
      Right products -> pure $ Just $ map _.id products
    productId <- case Array.head =<< productIds of
      Nothing -> Log.log Warn "Empty list of extracted products" *> pure Nothing
      Just productId -> pure $ Just productId
    pure { page, pageUrl, userId, productId, productIds }
  Api.sendEvent event
