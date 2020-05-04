module SweetSpot.Route where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromMaybe)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Prelude (class Eq, class Show, Unit, (#), ($), (<>))
import Routing.Duplex (RouteDuplex', optional, param, parse, path, record, root, segment, (:=))
import Routing.Duplex.Generic (product, sum)
import Routing.Hash (matchesWith)

-- toHistoryState :: { page :: Route } -> { page :: String }
-- toHistoryState { page } = case page of
--   Home -> { page: "home" }
--   Campaign campaignId -> { page: "campaign-" <> campaignId }

-- useEffect route do
--   Console.log $ "new route: " <> show route
--   History.pushState
--     (toHistoryState >>> Foreign.unsafeToForeign $ { page: route })
--     (DocumentTitle "SweetSpot Dashboard")
--     (URL currentUrl)
--     history
--   pure mempty

data Route
  = CampaignList { session :: Maybe String }
  | Campaign String { session :: Maybe String }

instance showRoute :: Show Route where
  show (CampaignList { session }) = "Campaign list" <> ", session: " <> (fromMaybe "missing session" session)
  show (Campaign campaignId { session }) = "Campaign" <> campaignId <> (fromMaybe "messing session" session)

derive instance genericRoute :: Generic Route _

derive instance eqRoute :: Eq Route

routeCodec :: RouteDuplex' Route
routeCodec =
  root
    $ sum
        { "CampaignList": path "campaign-list" (record # _session := optional (param "session"))
        -- , "Campaign": (path "campaign" ? { session: optional })
        , "Campaign": product (path "campaign" segment) (record # _session := optional (param "session"))
        }
  where
  _session = SProxy :: SProxy "session"

hoistRouter :: (Route -> Effect Unit) -> Effect (Effect Unit)
hoistRouter setRoute =
  matchesWith (parse routeCodec) \_ newRoute -> case newRoute of
    CampaignList sessionId -> setRoute (CampaignList sessionId)
    Campaign campaignId sessionId -> setRoute (Campaign campaignId sessionId)
