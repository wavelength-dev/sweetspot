module SweetSpot.State
  ( AppState(..)
  , Action(..)
  , Dispatch
  , RemoteState
  , initialState
  , fetchRemoteState
  , reducer
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff)
import SweetSpot.Data.Api (Product, UICampaign)
import SweetSpot.Route (Route(..))
import SweetSpot.Service (fetchCampaigns, fetchProducts)

type AppState =
  { products :: Array Product
  , campaigns :: Array UICampaign
  , route :: Route
  }

type RemoteState =
  { products :: Array Product
  , campaigns :: Array UICampaign
  }

data Action
  = Populate RemoteState
  | Navigate Route

type Dispatch = Action -> Effect Unit

initialState :: AppState
initialState = { products: [], campaigns: [], route: Home }

fetchRemoteState :: Aff RemoteState
fetchRemoteState = combine <$> fetchProducts <*> fetchCampaigns
  where
    combine =
      case _, _ of
        Right ps, Right cs -> { products: ps, campaigns: cs }
        _, _ -> { products: [], campaigns: [] }

reducer :: AppState -> Action -> AppState
reducer state = case _ of
  Populate { products, campaigns } ->
    state { products = products, campaigns = campaigns }
  Navigate route -> state { route = route }
