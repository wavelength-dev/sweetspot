module Supple.Data.Route where

import Prelude hiding ((/))

import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Number (fromString)
import Data.Number.Format (toString)
import Routing.Duplex (RouteDuplex', as, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = HomeRoute
  | ExperimentRoute Number
  | CreateRoute


derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "HomeRoute": "dashboard" / noArgs
  , "CreateRoute": "experiment" / "create" / noArgs
  , "ExperimentRoute": "experiment" / experimentId segment
  }

experimentId :: RouteDuplex' String -> RouteDuplex' Number
experimentId = as toString (fromString >>> note "Bad experiment id")
