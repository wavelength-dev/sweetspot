module SweetSpot.State
  ( AppState(..)
  , Action(..)
  , initialState
  , populateAppState
  , reducer
  ) where

import Prelude
import SweetSpot.Data.Api
import SweetSpot.Service

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, Fiber, runAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import SweetSpot.Route (Route(..))

data AppState = AppState
  { products :: Array Product
  , campaigns :: Array UICampaign
  , route :: Route
  }

data Action
  = Populate AppState

initialState :: AppState
initialState = AppState { products: [], campaigns: [], route: Home }

populateAppState :: Aff AppState
populateAppState = do
  liftEffect $ log "fetching"
  initAff
  where
    combine =
      case _, _, _ of
        Right ps', Right cs', route' -> AppState { products: ps', campaigns: cs', route: route' }
        _, _, _ -> initialState

    initAff = combine <$> fetchProducts <*> fetchCampaigns <*> pure Home


reducer :: AppState -> Action -> AppState
reducer state = case _ of
  Populate state' -> state'
