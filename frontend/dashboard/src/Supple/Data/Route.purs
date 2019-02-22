module Supple.Data.Route where

import Prelude hiding ((/))

import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (decimal, fromString, toStringAs)
import Routing.Duplex (RouteDuplex', as, root, segment, path)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = Home
  | Experiment Int
  | Create


derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home": "dashboard" / noArgs
  , "Create": "experiment" / "create" / noArgs
  , "Experiment": "experiment" / experimentId segment
  }

experimentId :: RouteDuplex' String -> RouteDuplex' Int
experimentId = as (toStringAs decimal) (fromString >>> note "Bad experiment id")
