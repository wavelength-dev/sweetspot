module Fulcrum.RuntimeDependency (getIsRuntimeAdequate) where

import Prelude
import Effect (Effect)

getIsRuntimeAdequate :: Effect Boolean
getIsRuntimeAdequate = (&&) <$> getIsPromiseRunnable <*> getIsFetchRunnable

foreign import getIsFetchRunnable :: Effect Boolean
foreign import getIsPromiseRunnable :: Effect Boolean
