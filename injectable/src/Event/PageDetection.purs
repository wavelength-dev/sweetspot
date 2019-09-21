module SweetSpot.Event.PageDetection (getCurrentPage, pageFromPathname) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex)
import Data.String.Regex (test) as Regex
import Data.String.Regex.Flags (ignoreCase) as RegexFlags
import Data.String.Regex.Unsafe (unsafeRegex) as RegexUnsafe
import Effect (Effect)
import SweetSpot.Data.Event (Page(..))
import Web.HTML (window) as HTML
import Web.HTML.Location (pathname) as Location
import Web.HTML.Window (location) as Window

collectionUrlRegex :: Regex
collectionUrlRegex = RegexUnsafe.unsafeRegex """^/collections/[\w-_.%~]+/?$""" RegexFlags.ignoreCase

collectionsUrlRegex :: Regex
collectionsUrlRegex = RegexUnsafe.unsafeRegex """^/collections/?$""" RegexFlags.ignoreCase

productDetailsUrlRegex :: Regex
productDetailsUrlRegex = RegexUnsafe.unsafeRegex """^/collections/[\w-_.%~]+/products/?""" RegexFlags.ignoreCase

ordersUrlRegex :: Regex
ordersUrlRegex = RegexUnsafe.unsafeRegex """^/[\w-_.%~]+/orders/?""" RegexFlags.ignoreCase

checkoutUrlRegex :: Regex
checkoutUrlRegex = RegexUnsafe.unsafeRegex """^/[\w-_.%~]+/checkouts/?""" RegexFlags.ignoreCase

homeUrlRegex :: Regex
homeUrlRegex = RegexUnsafe.unsafeRegex """^/?$""" RegexFlags.ignoreCase

cartUrlRegex :: Regex
cartUrlRegex = RegexUnsafe.unsafeRegex """^/cart/?""" RegexFlags.ignoreCase

getCurrentPage :: Effect (Maybe Page)
getCurrentPage = HTML.window >>= Window.location >>= Location.pathname >>= pageFromPathname >>> pure

pageFromPathname :: String -> Maybe Page
pageFromPathname path
  | Regex.test cartUrlRegex path = Just Cart
  | Regex.test collectionUrlRegex path = Just Collection
  | Regex.test collectionsUrlRegex path = Just Collections
  | Regex.test productDetailsUrlRegex path = Just Product
  | Regex.test checkoutUrlRegex path || Regex.test ordersUrlRegex path = Just Checkout
  | Regex.test homeUrlRegex path = Just Home
  | otherwise = Nothing
