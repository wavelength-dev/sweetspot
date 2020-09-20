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

logInfo :: String -> Effect Unit
logInfo = case Env.appEnv of
  Local -> Console.info
  Remote -> Datadog.logInfo

logWarn :: String -> Effect Unit
logWarn = case Env.appEnv of
  Local -> Console.warn
  Remote -> Datadog.logWarn

logError :: String -> Effect Unit
logError = case Env.appEnv of
  Local -> Console.error
  Remote -> Datadog.logError

logInfoContext :: forall a. String -> a -> Effect Unit
logInfoContext msg context = case Env.appEnv of
  Local -> runEffectFn2 jsLogInfo msg context
  Remote -> Datadog.logInfoContext msg context

logWarnContext :: forall a. String -> a -> Effect Unit
logWarnContext msg context = case Env.appEnv of
  Local -> runEffectFn2 jsLogWarn msg context
  Remote -> Datadog.logWarnContext msg context

logErrorContext :: forall a. String -> a -> Effect Unit
logErrorContext msg context = case Env.appEnv of
  Local -> runEffectFn2 jsLogError msg context
  Remote -> Datadog.logErrorContext msg context
