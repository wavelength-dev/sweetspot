module Datadog where

import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Foreign.Object (Object)
import Foreign.Object (empty) as Object
import Prelude (Unit)

foreign import logInfoImpl :: forall a. EffectFn2 String (Object a) Unit

foreign import logWarnImpl :: forall a. EffectFn2 String (Object a) Unit

foreign import logErrorImpl :: forall a. EffectFn2 String (Object a) Unit

logInfo :: String -> Effect Unit
logInfo msg = runEffectFn2 logInfoImpl msg Object.empty

logWarn :: String -> Effect Unit
logWarn msg = runEffectFn2 logWarnImpl msg Object.empty

logError :: String -> Effect Unit
logError msg = runEffectFn2 logErrorImpl msg Object.empty

logInfoContext :: forall a. String -> Object a -> Effect Unit
logInfoContext = runEffectFn2 logInfoImpl

logWarnContext :: forall a. String -> Object a -> Effect Unit
logWarnContext = runEffectFn2 logWarnImpl

logErrorContext :: forall a. String -> Object a -> Effect Unit
logErrorContext = runEffectFn2 logErrorImpl
