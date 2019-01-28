module Capability.Navigate where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Route (Route)
import Halogen (HalogenM)

-- | This capability represents the ability to move around the application. The `navigate` function
-- | should change the browser location, which will then notify our routing component. The `logout`
-- | function should clear any information associated with the user from the app and browser before
-- | redirecting them to the homepage.
class Monad m <= Navigate m where
  navigate :: Route -> m Unit

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance navigateHalogenM :: Navigate m => Navigate (HalogenM s f g p o m) where
  navigate = lift <<< navigate
