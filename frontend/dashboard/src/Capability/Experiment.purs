module Capability.Experiment where

import Prelude
import Halogen (HalogenM, lift)
import Data.Api (Experiment)
import Data.Maybe (Maybe)

class Monad m <= ManageExperiments m where
  getExperiments :: m (Maybe (Array Experiment))

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageArticleHalogenM
  :: ManageExperiments m
  => ManageExperiments (HalogenM s f g p o m) where
  getExperiments = lift getExperiments
