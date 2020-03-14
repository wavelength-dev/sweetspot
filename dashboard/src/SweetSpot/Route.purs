module SweetSpot.Route
  ( Route(..)
  , hoistRouter
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Routing.Duplex (RouteDuplex', path, root, segment, string, parse)
import Routing.Duplex.Generic as G
import Routing.Hash (matchesWith)

data Route = Home | Campaign String

instance showRoute :: Show Route where
  show Home = "Home"
  show (Campaign cmpId) = "Campaign" <> cmpId

derive instance genericRoute :: Generic Route _

route :: RouteDuplex' Route
route = root $ G.sum
  { "Home": G.noArgs
  , "Campaign": path "campaign" (string segment)
  }

hoistRouter ::  (Route -> Effect Unit) -> Effect (Effect Unit)
hoistRouter navigateTo =
  matchesWith (parse route) \_ new ->
    case new of
      Home -> navigateTo Home
      (Campaign s) -> navigateTo (Campaign s)
