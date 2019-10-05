module SweetSpot.Debug where

import Prelude

import Data.Maybe (maybe) as Maybe
import Effect (Effect)
import SweetSpot.SiteCapabilities (getUrlParam) as SiteC

type DebugMode
  = Boolean

getDebugMode :: Effect DebugMode
getDebugMode = SiteC.getUrlParam "ssdebug" >>= Maybe.maybe false ((==) "true") >>> pure
