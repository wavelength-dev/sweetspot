module Fulcrum.Logger where

import Prelude

import Data.Maybe as Maybe
import Datadog (logError, logErrorContext, logInfo, logInfoContext, logWarn, logWarnContext) as Datadog
import Effect (Effect)
import Effect.Console (error, info, warn) as Console
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Fulcrum.Site (getUrlParam) as Site

foreign import jsLogInfo :: forall a. EffectFn2 String a Unit

foreign import jsLogWarn :: forall a. EffectFn2 String a Unit

foreign import jsLogError :: forall a. EffectFn2 String a Unit

data LogLevel
  = Info
  | Warn
  | Error

logWithContext :: forall a. LogLevel -> String -> a -> Effect Unit
logWithContext level message context = do
  -- for convenience one can add a query parameter ssdebug to see logs
  -- in console
  isDebugging <- getIsDebugging
  when isDebugging case level of
    Info -> runEffectFn2 jsLogInfo message context
    Warn -> runEffectFn2 jsLogWarn message context
    Error -> runEffectFn2 jsLogError message context
  case level of
    Info -> Datadog.logInfoContext message context
    Warn -> Datadog.logWarnContext message context
    Error -> Datadog.logErrorContext message context

log :: LogLevel -> String -> Effect Unit
log level message = do
  -- for convenience one can add a query parameter ssdebug to see logs
  -- in console
  isDebugging <- getIsDebugging
  when isDebugging case level of
    Info -> Console.info message
    Warn -> Console.warn message
    Error -> Console.error message
  case level of
    Info -> Datadog.logInfo message
    Warn -> Datadog.logWarn message
    Error -> Datadog.logError message

getIsDebugging :: Effect Boolean
getIsDebugging = Site.getUrlParam "ssdebug" >>= Maybe.isJust >>> pure
