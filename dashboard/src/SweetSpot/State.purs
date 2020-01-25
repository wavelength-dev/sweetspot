module SweetSpot.State
  ( AppState(..)
  , Action(..)
  , initialState
  , fetchAppState
  , reducer
  ) where

import Prelude
import SweetSpot.Data.Api
import SweetSpot.Service

import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import SweetSpot.Route (Route(..))

type AppState =
  { products :: Array Product
  , campaigns :: Array UICampaign
  , route :: Route
  }


data Action
  = Populate AppState

initialState :: AppState
initialState = { products: [], campaigns: [], route: Home }

fetchAppState :: Aff AppState
fetchAppState = do
  liftEffect $ log "fetching"
  initAff
  where
    combine =
      case _, _, _ of
        Right ps', Right cs', route' -> { products: ps', campaigns: cs', route: route'  }
        _, _, _ -> initialState

    initAff = combine <$> fetchProducts <*> fetchCampaigns <*> pure Home


reducer :: AppState -> Action -> AppState
reducer state = case _ of
  Populate state' -> state'
