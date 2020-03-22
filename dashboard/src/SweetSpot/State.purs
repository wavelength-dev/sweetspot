module SweetSpot.State
  ( AppState(..)
  , Action(..)
  , Dispatch
  , RemoteState
  , mkInitialState
  , fetchRemoteState
  , reducer
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import SweetSpot.Data.Api (Product, UICampaign)
import SweetSpot.Route (Route(..))
import SweetSpot.Service (fetchCampaigns)

type AppState
  = { products :: Array Product
    , campaigns :: Array UICampaign
    , route :: Route
    , sessionId :: String
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

mkInitialState :: String -> AppState
mkInitialState sessionId =
  { products: []
  , campaigns: []
  , route: Home
  , sessionId: sessionId
  , shopName: Nothing
  }

fetchRemoteState :: String -> Aff RemoteState
fetchRemoteState session = combine <$> pure (Right []) <*> fetchCampaigns session
  where
  combine = case _, _ of
    Right ps, Right cs -> RemoteState { products: ps, campaigns: cs }
    _, _ -> RemoteStateError

reducer :: AppState -> Action -> AppState
reducer state action = case action of
  UpdateRemoteState RemoteStateError -> state { products = [], campaigns = [] }
  UpdateRemoteState (RemoteState { products, campaigns }) ->
    state
      { products = products
      , campaigns = campaigns
      }
  Navigate route ->
    state
      { route = route }
