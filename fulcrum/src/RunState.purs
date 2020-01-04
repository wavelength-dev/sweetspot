module Fulcrum.RunState where

import Prelude
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar (empty) as AVar
import Effect.Aff (Aff)

type ApplyDynamicPriceEffect
  = Aff Unit

foreign import getRunQueue :: Effect (AVar ApplyDynamicPriceEffect)

foreign import setRunQueue :: AVar ApplyDynamicPriceEffect -> Effect Unit

initRunQueue :: Effect Unit
initRunQueue = AVar.empty >>= setRunQueue

foreign import getIsRunning :: Effect Boolean

foreign import setIsRunning :: Boolean -> Effect Unit
