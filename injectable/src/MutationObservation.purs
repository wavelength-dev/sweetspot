module SweetSpot.MutationObservation where

import Prelude

import Effect (Effect)
import Web.DOM.MutationObserver (MutationObserver)
import Web.DOM.MutationRecord (MutationRecord)

type MutationCallback
  = Array MutationRecord -> MutationObserver -> Effect Unit
