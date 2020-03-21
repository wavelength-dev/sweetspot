module SweetSpot.Main where

import Prelude
import Assets (mountainCode) as Assets
import Data.Array (null) as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import React.Basic.DOM as R
import React.Basic.Hooks (JSX, ReactComponent, component, element, useEffect, useReducer, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import SweetSpot.Route (Route(..), hoistRouter)
import SweetSpot.Shopify as Shopify
import SweetSpot.State (Action(..), AppState, fetchRemoteState, mkInitialState, reducer)
import SweetSpot.Data.Api (UICampaign(..))
import SweetSpot.ExperimentListPage (ExperimentCardProps, ExperimentStatus(..), mkExperimentListPage)
import SweetSpot.QueryString as QS
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Location (search)
import Web.HTML.Window (document, location)

gettingStartedPage :: AppState -> JSX
gettingStartedPage { shopName } =
  let
    heading = case shopName of
      Just name -> "Hi " <> name <> ", Discover more profitable prices for your products"
      Nothing -> "Discover more profitable prices for your products"
  in
    element Shopify.emptyState
      { heading: heading
      , action: { content: "Create Price Experiment", onAction: mempty }
      , image: Assets.mountainCode
      , children: [ R.text "Here you'll create new price tests, check their progress, or their outcome." ]
      }

uiCampaignToExperimentCard :: UICampaign -> ExperimentCardProps
uiCampaignToExperimentCard (UICampaign { _uiCampaignName, _uiCampaignStart }) =
  { title: _uiCampaignName
  , creationDate: fromMaybe "DRAFT" _uiCampaignStart
  -- TODO: some localized date math
  , status: Running
  }

mkApp :: Effect (ReactComponent {})
mkApp = do
  qs <- window >>= location >>= search
  experimentListPage <- mkExperimentListPage
  let
    params = QS.parseQueryString qs

    mSessionId = QS.findParam "session" params
  sessionId <- case mSessionId of
    Just session -> pure session
    -- TODO: Figure out a better way to handle this case
    Nothing -> throw "Unable to parse sessionId"
  component "App" \props -> React.do
    state /\ dispatch <- useReducer (mkInitialState sessionId) reducer
    mState <- useAff "appState" (fetchRemoteState sessionId)
    useEffect "router" $ hoistRouter (dispatch <<< Navigate)
    useEffect (React.UnsafeReference mState) do
      case mState of
        Just (Right st) -> pure $ dispatch $ Populate st
        _ -> mempty
    pure
      $ element Shopify.appProvider
          { i18n: Shopify.enTranslations
          , children:
              if Array.null state.campaigns then
                gettingStartedPage state
              else
                element experimentListPage
                  { experiments: (map uiCampaignToExperimentCard state.campaigns)
                  , onCreateExperiment: dispatch (Navigate Home)
                  }
          }

main :: Effect Unit
main = do
  mAppElement <- getElementById "app" =<< (map toNonElementParentNode $ document =<< window)
  case mAppElement of
    Nothing -> throw "app element not found."
    Just appElement -> do
      app <- mkApp
      render (element app {}) appElement
