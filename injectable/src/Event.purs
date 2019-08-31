module SweetSpot.Event (trackView) where

import Prelude

import Data.Array as A
import Data.Either (hush)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import SweetSpot.AppM (getUserId)
import SweetSpot.Data.Codec (decodeProduct)
import SweetSpot.Data.Config (productClass)
import SweetSpot.Data.Event (Page(..))
import SweetSpot.Data.Shopify (Product)
import SweetSpot.Api (postEventPayload)
import Web.DOM.Document as D
import Web.DOM.Element as E
import Web.DOM.HTMLCollection (toArray)
import Web.DOM.Internal.Types (Element)
import Web.DOM.Node as N
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Location (href, pathname)
import Web.HTML.Window (document, location)

extractInjectedProductJSON :: Element -> Effect (Maybe Product)
extractInjectedProductJSON el = do
  text <- N.textContent $ E.toNode el
  pure $ hush $ decodeProduct text

readInjectedProducts :: Effect (Maybe (Array Product))
readInjectedProducts = do
  maybePs <- do
      doc <- window >>= document
      coll <- D.getElementsByClassName productClass (toDocument doc)
      els <- toArray coll
      sequence $ extractInjectedProductJSON <$> els

  pure $ sequence maybePs

collectionUrlRegex :: Regex
collectionUrlRegex = unsafeRegex """^/collections/[\w-_.%~]+/?$""" ignoreCase

collectionsUrlRegex :: Regex
collectionsUrlRegex = unsafeRegex """^/collections/?$""" ignoreCase

productDetailsUrlRegex :: Regex
productDetailsUrlRegex = unsafeRegex """^/collections/[\w-_.%~]+/products/?""" ignoreCase

ordersUrlRegex :: Regex
ordersUrlRegex = unsafeRegex """^/[\w-_.%~]+/orders/?""" ignoreCase

checkoutUrlRegex :: Regex
checkoutUrlRegex = unsafeRegex """^/[\w-_.%~]+/checkouts/?""" ignoreCase

homeUrlRegex :: Regex
homeUrlRegex = unsafeRegex "^/?$" ignoreCase

detectPage :: Effect Page
detectPage = window >>= location >>= pathname >>= pure <<< getPage
  where
    getPage :: String -> Page
    getPage path
      | test collectionUrlRegex path = Collection
      | test collectionsUrlRegex path = Collections
      | test productDetailsUrlRegex path = Product
      | test checkoutUrlRegex path || test ordersUrlRegex path = Checkout
      | test homeUrlRegex path = Home
      | otherwise = Unknown

trackView :: Aff Unit
trackView = do
  viewEvent <- liftEffect $ do
    mUserId <- getUserId
    let userId = unwrap <$> mUserId
    page <- detectPage
    pageUrl <- window >>= location >>= href
    products <- readInjectedProducts
    let
      productIds = (map _.id) <$> products
      productId = productIds >>= A.head
    pure { page, pageUrl, userId, productId, productIds }
  postEventPayload viewEvent
