module SweetSpot.Logger where

import Datadog (logError, logErrorContext, logInfo, logInfoContext, logWarn, logWarnContext) as Datadog
import Effect (Effect)
import Effect.Console as Console
import Prelude (Unit)
import SweetSpot.Env (AppEnv(..))
import SweetSpot.Env as Env

logInfo :: String -> Effect Unit
logInfo = case Env.appEnv of
  Local -> Console.info
  Remote -> Datadog.logInfo

logWarn :: String -> Effect Unit
logWarn = case Env.appEnv of
  Local -> Console.info
  Remote -> Datadog.logWarn

logError :: String -> Effect Unit
logError = case Env.appEnv of
  Local -> Console.info
  Remote -> Datadog.logError

logInfoContext :: forall a. String -> Record a -> Effect Unit
logInfoContext msg context = case Env.appEnv of
  Local -> Console.info msg
  Remote -> Datadog.logInfoContext msg context

logWarnContext :: forall a. String -> Record a -> Effect Unit
logWarnContext msg context = case Env.appEnv of
  Local -> Console.info msg
  Remote -> Datadog.logWarnContext msg context

logErrorContext :: forall a. String -> Record a -> Effect Unit
logErrorContext msg context = case Env.appEnv of
  Local -> Console.info msg
  Remote -> Datadog.logErrorContext msg context
