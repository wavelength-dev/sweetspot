module SweetSpot.Route
  ( Route(..)
  , hoistRouter
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Routing.Duplex (RouteDuplex', path, root, segment, string, parse)
import Routing.Duplex.Generic as G
import Routing.PushState (makeInterface, matchesWith)

data Route = Home | Profile String

instance showRoute :: Show Route where
  show Home = "Home"
  show (Profile p) = "Profile " <> p

derive instance genericRoute :: Generic Route _

route :: RouteDuplex' Route
route = root $ G.sum
  { "Home": G.noArgs
  , "Profile": path "profile" (string segment)
  }

hoistRouter :: ((Route -> Route) -> Effect Unit) -> Effect (Effect Unit)
hoistRouter setPage = do
  nav <- makeInterface
  nav # matchesWith (parse route) \_ new ->
    case new of
      Home -> setPage $ const Home
      (Profile s) -> setPage $ const (Profile s)
