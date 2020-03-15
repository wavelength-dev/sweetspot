module SweetSpot.QueryString where

import Prelude

import Data.Either (Either(..))
import Data.Lens (_Just, _Right, filtered, firstOf, folded, to)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, stripPrefix) as String
import Global (decodeURIComponent)

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
    >>> splitOnAmpersand
    >>> map splitOnEquals
    >>> map rawToQueryParam
  where
  stripQuestionMark = \str -> (String.stripPrefix (String.Pattern "?") str) # fromMaybe str

  splitOnAmpersand = String.split (String.Pattern "&")

  splitOnEquals = String.split (String.Pattern "=")

  -- parsing query parameters is hard, consider using lib
  rawToQueryParam :: Array String -> Either String QueryParam
  rawToQueryParam = case _ of
    [ key ] -> Left "Parameter without an '='"
    [ "", _ ] -> Left "Parameter without a key"
    [ key, "" ] -> Right $ QueryParam key Nothing
    [ key, value ] -> Right $ QueryParam key (decodeURIComponent value)
    _ -> Left "More than one '=' in single parameter"

findParam :: Key -> Array (Either String QueryParam) -> Maybe String
findParam key =
  firstOf $
    folded
    <<< _Right
    <<< filtered (\(QueryParam k _) -> k == key)
    <<< to (\(QueryParam _ v) -> v)
    <<< _Just
