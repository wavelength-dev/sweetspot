module SweetSpot.SiteCapabilities.Url (getPathname, getUrlParam_, replacePathname) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (oneOf)
import Effect (Effect)
import SweetSpot.QueryString (QueryParam(..))
import SweetSpot.QueryString (parseQueryString) as QueryString
import Web.HTML (window)
import Web.HTML.History (DocumentTitle(..), URL(..))
import Web.HTML.History (replaceState, state) as History
import Web.HTML.Location (pathname, search) as Location
import Web.HTML.Window as Window

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
