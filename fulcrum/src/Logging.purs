module Fulcrum.Logging where

import Prelude

import Data.Maybe as Maybe
import Datadog (logError, logErrorContext, logInfo, logInfoContext, logWarn, logWarnContext) as Datadog
import Effect (Effect)
import Effect.Console (error, info, warn) as Console
import Foreign.Object as Object
import Fulcrum.Site (getUrlParam) as Site
import Type.Row.Homogeneous (class Homogeneous)

data LogLevel
  = Info
  | Warn
  | Error

logWithContext :: forall r a. Homogeneous a r => LogLevel -> String -> Record a -> Effect Unit
logWithContext level message context = do
  -- for convenience one can add a query parameter ssdebug to see logs
  -- in console
  isDebugging <- getIsDebugging
  when isDebugging case level of
    Info -> message # show >>> Console.info
    Warn -> message # show >>> Console.warn
    Error -> message # show >>> Console.error
  case level of
    Info -> Datadog.logInfoContext message (Object.fromHomogeneous context)
    Warn -> Datadog.logWarnContext message (Object.fromHomogeneous context)
    Error -> Datadog.logErrorContext message (Object.fromHomogeneous context)

log :: LogLevel -> String -> Effect Unit
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
