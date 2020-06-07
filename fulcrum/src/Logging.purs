module Fulcrum.Logging (log, LogLevel(..)) where

import Prelude

import Data.Argonaut (class EncodeJson)
import Data.Argonaut (encodeJson) as Argonaut
import Data.Maybe as Maybe
import Effect (Effect)
import Datadog (logError, logInfo, logWarn) as Datadog
import Effect.Console (error, info, warn) as Console
import Fulcrum.Site (getUrlParam) as Site

data LogLevel
  = Info
  | Warn
  | Error

instance showLogLevel :: Show LogLevel where
  show Info = "info"
  show Warn = "warn"
  show Error = "error"

instance encodeJsonLogLevel :: EncodeJson LogLevel where
  encodeJson Info = Argonaut.encodeJson "info"
  encodeJson Warn = Argonaut.encodeJson "warn"
  encodeJson Error = Argonaut.encodeJson "error"

log :: forall a. Show a => LogLevel -> a -> Effect Unit
log level message = do
  -- for convenience one can add a query parameter ssdebug to see logs
  -- in console
  isDebugging <- getIsDebugging
  when isDebugging case level of
    Info -> message # show >>> Console.info
    Warn -> message # show >>> Console.warn
    Error -> message # show >>> Console.error
  case level of
       Info -> message # show >>> Datadog.logInfo
       Warn -> message # show >>> Datadog.logWarn
       Error -> message # show >>> Datadog.logError

getIsDebugging :: Effect Boolean
getIsDebugging = Site.getUrlParam "ssdebug" >>= Maybe.isJust >>> pure
