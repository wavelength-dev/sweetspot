module SweetSpot.Logger where

import Prelude
import Datadog (logError, logErrorContext, logInfo, logInfoContext, logWarn, logWarnContext) as Datadog
import Effect (Effect)
import Effect.Console as Console
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Foreign.Object (Object)
import Foreign.Object as Object
import SweetSpot.Env (AppEnv(..))
import SweetSpot.Env as Env
import Type.Row.Homogeneous (class Homogeneous)

foreign import jsLogInfo :: forall a. EffectFn2 String (Object a) Unit

foreign import jsLogWarn :: forall a. EffectFn2 String (Object a) Unit

foreign import jsLogError :: forall a. EffectFn2 String (Object a) Unit

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

logInfoContext :: forall r a. Homogeneous r a => String -> Record r -> Effect Unit
logInfoContext msg context =
  let
    contextObj = Object.fromHomogeneous context
  in
    case Env.appEnv of
      Local -> runEffectFn2 jsLogInfo msg contextObj
      Remote -> Datadog.logInfoContext msg contextObj

logWarnContext :: forall r a. Homogeneous r a => String -> Record r -> Effect Unit
logWarnContext msg context =
  let
    contextObj = Object.fromHomogeneous context
  in
    case Env.appEnv of
      Local -> runEffectFn2 jsLogWarn msg contextObj
      Remote -> Datadog.logWarnContext msg contextObj

logErrorContext :: forall r a. Homogeneous r a => String -> Record r -> Effect Unit
logErrorContext msg context =
  let
    contextObj = Object.fromHomogeneous context
  in
    case Env.appEnv of
      Local -> runEffectFn2 jsLogError msg contextObj
      Remote -> Datadog.logErrorContext msg contextObj
