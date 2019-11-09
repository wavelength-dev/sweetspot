module Fulcrum.Site (getUrlParam) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (oneOf)
import Effect (Effect)
import Fulcrum.QueryString (QueryParam(..))
import Fulcrum.QueryString (parseQueryString) as QueryString
import Web.HTML (window)
import Web.HTML.Location (search) as Location
import Web.HTML.Window as Window

getUrlParam :: String -> Effect (Maybe String)
getUrlParam targetKey = do
  queryString <- window >>= Window.location >>= Location.search
  queryString
    # QueryString.parseQueryString
    >>> map matchQueryParam
    >>> oneOf
    >>> pure
  where
  matchQueryParam :: Either String QueryParam -> Maybe String
  matchQueryParam eQueryParam =
    case eQueryParam of
      Right (QueryParam key (Just value)) ->
        if key == targetKey then
          Just value
        else
          Nothing
      _ -> Nothing
