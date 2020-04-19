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
  show (Campaign campaignId) = "Campaign" <> campaignId

derive instance genericRoute :: Generic Route _

route :: RouteDuplex' Route
route = root $ G.sum
  { "Home": G.noArgs
  , "Campaign": path "campaign" (string segment)
  }

hoistRouter ::  (Route -> Effect Unit) -> Effect (Effect Unit)
hoistRouter setRoute =
  matchesWith (parse route) \_ newRoute ->
    case newRoute of
      Home -> setRoute Home
      Campaign campaignId -> setRoute (Campaign campaignId)
