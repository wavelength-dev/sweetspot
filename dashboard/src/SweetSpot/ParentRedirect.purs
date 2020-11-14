module SweetSpot.ParentRedirect where

import Prelude
import Effect (Effect)

foreign import parentRedirect :: String -> Effect Unit
