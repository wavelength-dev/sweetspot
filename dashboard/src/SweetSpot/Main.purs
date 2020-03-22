module SweetSpot.Main where

import Prelude

import Data.Array (null) as Array
import Debug.Trace (traceM) as Debug
import Data.Lens (non, view)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import React.Basic.Hooks (ReactComponent, component, element, unsafeHook, useEffect, useReducer, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import SweetSpot.Data.Api (UICampaign, uiCampaignName, uiCampaignStart)
import SweetSpot.ExperimentListPage (ExperimentCardProps, ExperimentStatus(..), mkExperimentListPage)
import SweetSpot.GettingStartedPage (gettingStartedPage)
import SweetSpot.MissingSessionPage (mkMissingSessionPage)
import SweetSpot.QueryString as QS
import SweetSpot.Route (Route(..), hoistRouter)
import SweetSpot.Shopify as Shopify
import SweetSpot.State (Action(..), fetchRemoteState, mkInitialState, reducer)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Location (search)
import Web.HTML.Window (document, location)

uiCampaignToExperimentCard :: UICampaign -> ExperimentCardProps
uiCampaignToExperimentCard campaign =
  { title: view uiCampaignName campaign
  , creationDate: view (uiCampaignStart <<< non "DRAFT") campaign
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

    app sessionId =
      component "App" \props -> React.do
        state /\ dispatch <- useReducer (mkInitialState sessionId) reducer
        unsafeHook (Debug.traceM state)
        useAff sessionId do
          remoteState <- fetchRemoteState sessionId
          pure $ dispatch (UpdateRemoteState remoteState)
        useEffect unit (hoistRouter (dispatch <<< Navigate))
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
  case mSessionId of
    Just sessionId -> app sessionId
    Nothing -> mkMissingSessionPage

main :: Effect Unit
main = do
  documentNode <- window >>= document >>= toNonElementParentNode >>> pure
  mAppElement <- getElementById "app" documentNode
  case mAppElement of
    Nothing -> throw "app element not found."
    Just appElement -> do
      app <- mkApp
      render (element app {}) appElement
