module SweetSpot.Session where

import Prelude
import Data.Maybe (Maybe)
import Effect (Effect)
import SweetSpot.QueryString (findParam, parseQueryString) as QueryString
import Web.HTML (window) as HTML
import Web.HTML.Location (search) as Location
import Web.HTML.Window (location) as Window

newtype SessionId
  = SessionId String

derive instance eqSessionId :: Eq SessionId

getSessionId :: Effect (Maybe SessionId)
getSessionId =
  HTML.window
    >>= Window.location
    >>= Location.search
    >>= QueryString.parseQueryString
    >>> QueryString.findParam "session"
    >>> map SessionId
    >>> pure
