module Fulcrum.RunState where

import Prelude
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar (empty) as AVar
import Effect.Aff (Aff)

type ApplyExperimentEffect
  = Aff Unit

foreign import getRunQueue :: Effect (AVar ApplyExperimentEffect)

foreign import setRunQueue :: AVar ApplyExperimentEffect -> Effect Unit

initRunQueue :: Effect Unit
initRunQueue = AVar.empty >>= setRunQueue
