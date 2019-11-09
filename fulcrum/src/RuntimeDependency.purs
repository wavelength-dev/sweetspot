module Fulcrum.RuntimeDependency (getIsRuntimeAdequate) where

import Prelude
import Effect (Effect)

getIsRuntimeAdequate :: Effect Boolean
getIsRuntimeAdequate = do
  isFetchRunnable <- getIsFetchRunnable
  isPromiseRunnable <- getIsPromiseRunnable
  isFetchRunnable && isPromiseRunnable # pure

foreign import getIsFetchRunnable :: Effect Boolean
foreign import getIsPromiseRunnable :: Effect Boolean
