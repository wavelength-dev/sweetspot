module SweetSpot.Logger where

import Prelude
import Datadog (logError, logErrorContext, logInfo, logInfoContext, logWarn, logWarnContext) as Datadog
import Effect (Effect)
import Effect.Console as Console
import Effect.Uncurried (EffectFn2, runEffectFn2)
import SweetSpot.Env (AppEnv(..))
import SweetSpot.Env as Env

foreign import jsLogInfo :: forall a. EffectFn2 String a Unit

foreign import jsLogWarn :: forall a. EffectFn2 String a Unit

foreign import jsLogError :: forall a. EffectFn2 String a Unit

data LogLevel
  = Info
  | Warn
  | Error

log :: LogLevel -> String -> Effect Unit
log level = case level, Env.appEnv of
  Info, Local -> Console.info
  Warn, Local -> Console.warn
  Error, Local -> Console.error
  Info, Remote -> Datadog.logInfo
  Warn, Remote -> Datadog.logWarn
  Error, Remote -> Datadog.logError

logWithContext :: forall a. LogLevel -> String -> a -> Effect Unit
logWithContext level = case level, Env.appEnv of
  Info, Local -> runEffectFn2 jsLogInfo
  Warn, Local -> runEffectFn2 jsLogWarn
  Error, Local -> runEffectFn2 jsLogError
  Info, Remote -> Datadog.logInfoContext
  Warn, Remote -> Datadog.logWarnContext
  Error, Remote -> Datadog.logErrorContext
