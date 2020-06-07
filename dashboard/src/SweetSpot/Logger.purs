module SweetSpot.Logger where

import Datadog (logError, logErrorContext, logInfo, logInfoContext, logWarn, logWarnContext) as Datadog
import Effect (Effect)
import Effect.Console as Console
import Foreign.Object as Object
import Prelude (Unit)
import SweetSpot.Env (AppEnv(..))
import SweetSpot.Env as Env
import Type.Row.Homogeneous (class Homogeneous)

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

logInfoContext :: forall r a. Homogeneous r a => String -> Record r -> Effect Unit
logInfoContext msg context = case Env.appEnv of
  Local -> Console.info msg
  Remote -> Datadog.logInfoContext msg (Object.fromHomogeneous context)

logWarnContext :: forall r a. Homogeneous r a => String -> Record r -> Effect Unit
logWarnContext msg context = case Env.appEnv of
  Local -> Console.info msg
  Remote -> Datadog.logWarnContext msg (Object.fromHomogeneous context)

logErrorContext :: forall r a. Homogeneous r a => String -> Record r -> Effect Unit
logErrorContext msg context = case Env.appEnv of
  Local -> Console.info msg
  Remote -> Datadog.logErrorContext msg (Object.fromHomogeneous context)
