module SweetSpot.State where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff (parallel, sequential) as Aff
import Effect.Class (liftEffect)
import Effect.Console (error) as Console
import SweetSpot.Data.Api (Product, UICampaign)
import SweetSpot.Route (Route(..))
import SweetSpot.Service (fetchCampaigns, fetchProducts)
import SweetSpot.Session (SessionId)

type AppState
  = { products :: Array Product
    , campaigns :: Array UICampaign
    , route :: Route
    , sessionId :: SessionId
    , shopName :: Maybe String
    }

data RemoteState
  = RemoteStateError
  | RemoteState
    { products :: Array Product
    , campaigns :: Array UICampaign
    }

data Action
  = UpdateRemoteState RemoteState
  | Navigate Route

type Dispatch
  = Action -> Effect Unit

mkInitialState :: SessionId -> AppState
mkInitialState sessionId =
  { products: []
  , campaigns: []
  , route: Home
  , sessionId
  , shopName: Nothing
  }

fetchRemoteState :: SessionId -> Aff RemoteState
fetchRemoteState session = do
  Tuple eProducts eCampaigns <-
    Aff.sequential
      $ Tuple
      <$> Aff.parallel (fetchProducts session)
      <*> Aff.parallel (fetchCampaigns session)
  liftEffect case eProducts, eCampaigns of
    Left err, _ -> Console.error err *> pure RemoteStateError
    _, Left err -> Console.error err *> pure RemoteStateError
    Right products, Right campaigns -> { products, campaigns } # RemoteState >>> pure

appStateReducer :: AppState -> Action -> AppState
appStateReducer state action = case action of
  UpdateRemoteState RemoteStateError -> state
  UpdateRemoteState (RemoteState { products, campaigns }) ->
    state
      { products = products
      , campaigns = campaigns
      }
  Navigate route ->
    state
      { route = route }
