module SweetSpot.Route where

import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Prelude (class Eq, class Show, Unit, bind, pure, ($), (<>))
import Routing.Duplex (RouteDuplex', parse, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Hash (matchesWith)

data Route
  = CampaignList
  | CampaignView String
  | CampaignCreate

instance showRoute :: Show Route where
  show (CampaignList) = "Experiment List"
  show (CampaignView campaignId) = "Experiment View: " <> campaignId
  show (CampaignCreate) = "Experiment Create"

derive instance genericRoute :: Generic Route _

derive instance eqRoute :: Eq Route

routeCodec :: RouteDuplex' Route
routeCodec =
  root
    $ sum
        { "CampaignList": noArgs
        , "CampaignCreate": "campaign" / noArgs
        , "CampaignView": "campaign" / segment
        }

useRouter :: (Route -> Effect Unit) -> Effect (Effect Unit)
useRouter setRoute = do
  removeHashEventListener <-
    matchesWith (parse routeCodec) \oldRoute newRoute -> setRoute newRoute
  pure removeHashEventListener
