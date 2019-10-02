module SweetSpot.QueryString where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, stripPrefix) as String
import Global (decodeURIComponent)

-- function parseQuery(queryString) {
--     var query = {};
--     var pairs = (queryString[0] === '?' ? queryString.substr(1) : queryString).split('&');
--     for (var i = 0; i < pairs.length; i++) {
--         var pair = pairs[i].split('=');
--         query[decodeURIComponent(pair[0])] = decodeURIComponent(pair[1] || '');
--     }
--     return query;
-- }
type Key
  = String

type Value
  = String

data QueryParam
  = QueryParam Key (Maybe Value)

derive instance eqQueryParam :: Eq QueryParam

instance showQueryParam :: Show QueryParam where
  show (QueryParam key Nothing) = key <> "="
  show (QueryParam key (Just value)) = key <> "=" <> value

parseQueryString :: String -> Array (Either String QueryParam)
parseQueryString =
  stripQuestionMark
    >>> splitByAmpersand
    >>> map splitByEquals
    >>> map rawToQueryParam
  where
  stripQuestionMark = \str -> (String.stripPrefix (String.Pattern "?") str) # fromMaybe str

  splitByAmpersand = String.split (String.Pattern "&")

  splitByEquals = String.split (String.Pattern "=")

  -- parsing query parameters is hard, consider using lib
  rawToQueryParam :: Array String -> Either String QueryParam
  rawToQueryParam = case _ of
    [ key ] -> Left "Parameter without an '='"
    [ "", _ ] -> Left "Parameter without a key"
    [ key, "" ] -> Right $ QueryParam key Nothing
    [ key, value ] -> Right $ QueryParam key (decodeURIComponent value)
    _ -> Left "More than one '=' in single parameter"
