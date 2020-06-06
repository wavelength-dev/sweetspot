module Datadog where

import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Prelude (Unit)

foreign import logInfoImpl :: forall a. EffectFn2 String (Record a) Unit

foreign import logWarnImpl :: forall a. EffectFn2 String (Record a) Unit

foreign import logErrorImpl :: forall a. EffectFn2 String (Record a) Unit

logInfo :: String -> Effect Unit
logInfo msg = runEffectFn2 logInfoImpl msg {}

logWarn :: String -> Effect Unit
logWarn msg = runEffectFn2 logWarnImpl msg {}

logError :: String -> Effect Unit
logError msg = runEffectFn2 logErrorImpl msg {}

logInfoContext :: forall a. String -> Record a -> Effect Unit
logInfoContext = runEffectFn2 logInfoImpl

logWarnContext :: forall a. String -> Record a -> Effect Unit
logWarnContext = runEffectFn2 logWarnImpl

logErrorContext :: forall a. String -> Record a -> Effect Unit
logErrorContext = runEffectFn2 logErrorImpl
