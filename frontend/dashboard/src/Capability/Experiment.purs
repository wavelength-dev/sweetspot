module Supple.Capability.Experiment where

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Prelude
import Supple.Data.Api (Experiment)

class Monad m <= ManageExperiments m where
  getExperiments :: m (Maybe (Array Experiment))

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageArticleHalogenM
  :: ManageExperiments m
  => ManageExperiments (HalogenM s f g p o m) where
  getExperiments = lift getExperiments
