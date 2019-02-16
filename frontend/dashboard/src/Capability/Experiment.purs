module Supple.Capability.Experiment where

import Prelude

import Halogen (HalogenM, lift)
import Supple.Data.Api (ExperimentsResource, ProductsResource)

class Monad m <= ManageExperiments m where
  getExperiments :: m ExperimentsResource
  getProducts :: m ProductsResource

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageExperimentsHalogenM
  :: ManageExperiments m
  => ManageExperiments (HalogenM s f g p o m) where
  getExperiments = lift getExperiments
  getProducts = lift getProducts
