module Supple.Event (trackView) where

import Prelude

import Data.Array as A
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe)
import Data.String as S
import Data.String.Regex (Regex, regex, test)
import Data.String.Regex.Flags (ignoreCase)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (forkAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Supple.AppM (AppM)
import Supple.Capability (getUserId)
import Supple.Data.Api (UserBucket(..))
import Supple.Data.Codec (decodeProduct)
import Supple.Data.Constant (productClass)
import Supple.Data.Event (Page(..))
import Supple.Data.Shopify (Product)
import Supple.Request (postEventPayload)
import Web.DOM.Document as D
import Web.DOM.Element as E
import Web.DOM.HTMLCollection (toArray)
import Web.DOM.Internal.Types (Element)
import Web.DOM.Node as N
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Location (href, pathname, search)
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

collectionUrlRegex :: Either String Regex
collectionUrlRegex = regex "^/collections/\\w+$" ignoreCase

collectionsUrlRegex :: Either String Regex
collectionsUrlRegex = regex "^/collections$" ignoreCase

productDetailsUrlRegex :: Either String Regex
productDetailsUrlRegex = regex "^/collections/\\w+/products" ignoreCase

ordersUrlRegex :: Either String Regex
ordersUrlRegex = regex "^/\\w+/orders/" ignoreCase

checkoutUrlRegex :: Either String Regex
checkoutUrlRegex = regex "^/\\w+/checkouts/" ignoreCase

isCollectionUrl :: String -> Boolean
isCollectionUrl path =
  case collectionUrlRegex of
    Right r -> test r path
    Left _ -> false

isCollectionsUrl :: String -> Boolean
isCollectionsUrl path =
  case collectionsUrlRegex of
    Right r -> test r path
    Left _ -> false

isProductDetailsUrl :: String -> Boolean
isProductDetailsUrl path =
  case productDetailsUrlRegex of
    Right r -> test r path
    Left _ -> false

isCheckoutUrl :: String -> Boolean
isCheckoutUrl path =
  case regexes of
    Right (Tuple cr or) -> test cr path || test or path
    Left _ -> false
  where regexes = Tuple <$> checkoutUrlRegex <*> ordersUrlRegex

detectPage :: Effect Page
detectPage = window >>= location >>= pathname >>= pure <<< getPage
  where
    getPage :: String -> Page
    getPage path
      | isCollectionUrl path = Collection
      | isCollectionsUrl path = Collections
      | isProductDetailsUrl path = Product
      | isCheckoutUrl path = Checkout
      | otherwise = Unknown

trackView :: UserBucket -> AppM Unit
trackView (UserBucket { _ubExpId, _ubBucketId }) = do
  userId <- getUserId
  viewEvent <- liftEffect $ do
      page <- detectPage
      pageUrl <- window >>= location >>= href
      products <- readInjectedProducts
      let
        productIds = (map _.id) <$> products
        productId = productIds >>= A.head
      pure { page, pageUrl, expId: _ubExpId, userId, bucketId: _ubBucketId, productId, productIds }

  _ <- liftAff $ forkAff $ postEventPayload viewEvent
  pure unit
