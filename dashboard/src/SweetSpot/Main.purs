module SweetSpot.Main where

import Prelude

import Data.Array (null) as Array
import Data.DateTime (DateTime)
import Data.Either (Either, either)
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Now as Now
import React.Basic.DOM (render)
import React.Basic.DOM (text) as R
import React.Basic.Hooks (ReactComponent, component, element, useEffect, useState, (/\))
import React.Basic.Hooks (bind, discard) as React
import React.Basic.Hooks.Aff (useAff)
import SweetSpot.Data.Api (UICampaign, uiCampaignEnd, uiCampaignName, uiCampaignStart)
import SweetSpot.ExperimentListPage (ExperimentCardProps, ExperimentStatus(..), mkExperimentListPage)
import SweetSpot.GettingStartedPage (gettingStartedPage)
import SweetSpot.MissingSessionPage (mkMissingSessionPage)
import SweetSpot.Route (Route(..), hoistRouter)
import SweetSpot.Service (fetchCampaigns) as Service
import SweetSpot.Session (SessionId)
import SweetSpot.Session (getSessionId) as Session
import SweetSpot.Shopify as Shopify
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

type Now
  = DateTime

type CampaignEnd
  = DateTime

type CampaignStart
  = DateTime

toStatus :: Now -> Maybe CampaignEnd -> Maybe CampaignStart -> ExperimentStatus
toStatus _ Nothing Nothing = Draft

toStatus now (Just endDateTime) startDateTime =
  if endDateTime < now then
    Finished endDateTime
  else
    toStatus now Nothing startDateTime

toStatus now Nothing (Just startDateTime) =
  if startDateTime < now then
    Running startDateTime
  else
    Starting startDateTime

uiCampaignToExperimentCard :: Now -> UICampaign -> ExperimentCardProps
uiCampaignToExperimentCard now campaign = do
  let
    title = view uiCampaignName campaign
  let
    mStartDateTime = view uiCampaignStart campaign
  let
    mEndDateTime = view uiCampaignEnd campaign
  let
    status = toStatus now mEndDateTime mStartDateTime
  { title, status }

data RemoteResource a
  = Empty
  | Loading
  | Resource a
  | Error String

derive instance eqRemoteResource :: Eq a => Eq (RemoteResource a)

eitherToResource :: Either String ~> RemoteResource
eitherToResource = either Error Resource

mkApp :: Effect (ReactComponent { sessionId :: SessionId })
mkApp = do
  experimentListPage <- mkExperimentListPage
  now <- Now.nowDateTime
  component "App" \props -> React.do
    route /\ setRoute <- useState Home
    useEffect unit (hoistRouter $ \routeToNavigateTo -> setRoute (const routeToNavigateTo))
    campaignsResource /\ setCampaignsResource <- useState Empty
    useAff props.sessionId do
      liftEffect $ setCampaignsResource $ const Loading
      eCampaigns <- Service.fetchCampaigns props.sessionId
      eitherToResource eCampaigns
        # const
        >>> setCampaignsResource
        >>> liftEffect
      pure unit
    pure
      $ element Shopify.appProvider
          { i18n: Shopify.enTranslations
          , children:
              case campaignsResource of
                Empty -> R.text "EMPTY"
                Loading -> R.text "LOADING"
                Error err -> R.text ("ERROR: " <> err)
                Resource campaigns ->
                  if Array.null campaigns then
                    gettingStartedPage
                  else
                    element experimentListPage
                      { experiments: map (uiCampaignToExperimentCard now) campaigns
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
        render (element app { sessionId }) appElement
