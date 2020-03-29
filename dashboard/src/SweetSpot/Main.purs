module SweetSpot.Main where

import Prelude
import Data.Array (null) as Array
import Data.Lens (non, view)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import React.Basic.Hooks (ReactComponent, component, element, useEffect, useReducer, (/\))
import React.Basic.Hooks (bind, discard) as React
import React.Basic.Hooks.Aff (useAff)
import SweetSpot.Data.Api (UICampaign, uiCampaignName, uiCampaignStart)
import SweetSpot.ExperimentListPage (ExperimentCardProps, ExperimentStatus(..), mkExperimentListPage)
import SweetSpot.GettingStartedPage (gettingStartedPage)
import SweetSpot.MissingSessionPage (mkMissingSessionPage)
import SweetSpot.Route (hoistRouter)
import SweetSpot.Session (SessionId)
import SweetSpot.Session (getSessionId) as Session
import SweetSpot.Shopify as Shopify
import SweetSpot.State (Action(..), appStateReducer, fetchRemoteState, mkInitialState)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

uiCampaignToExperimentCard :: UICampaign -> ExperimentCardProps
uiCampaignToExperimentCard campaign =
  { title: view uiCampaignName campaign
  , creationDate: view (uiCampaignStart <<< non "DRAFT") campaign
  -- TODO: some localized date math
  , status: Running
  }

mkApp :: Effect (ReactComponent { sessionId :: SessionId })
mkApp = do
  experimentListPage <- mkExperimentListPage
  component "App" \props -> React.do
    state /\ dispatch <- useReducer (mkInitialState props.sessionId) appStateReducer
    useAff state.sessionId do
      remoteState <- fetchRemoteState state.sessionId
      _ <- liftEffect $ dispatch (UpdateRemoteState remoteState)
      pure unit
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
                  , onCreateExperiment: mempty
                  }
          }

main :: Effect Unit
main = do
  mSessionId <- Session.getSessionId
  documentNode <- window >>= document >>= toNonElementParentNode >>> pure
  mAppElement <- getElementById "app" documentNode
  missingSessionPage <- mkMissingSessionPage
  case mAppElement of
    Nothing -> throw "app element not found."
    Just appElement -> case mSessionId of
      Nothing -> render (element missingSessionPage {}) appElement
      Just sessionId -> do
        app <- mkApp
        render (element app {sessionId}) appElement
