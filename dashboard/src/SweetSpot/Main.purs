module SweetSpot.Main where

import Prelude
import Data.Array (null) as Array
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import React.Basic.DOM (text) as R
import React.Basic.Hooks (Component, component, element, useState, (/\))
import React.Basic.Hooks (bind, discard) as React
import React.Basic.Hooks.Aff (useAff)
import SweetSpot.ExperimentListPage (mkExperimentListPage)
import SweetSpot.ExperimentPage (mkExperimentPage)
import SweetSpot.GettingStartedPage (gettingStartedPage)
import SweetSpot.MissingSessionPage (missingSessionPage)
import SweetSpot.Mock (expensiveJacketsCheapMonkeysCampaign, storewide10Campaign)
import SweetSpot.Routerless (Route(..))
import SweetSpot.Service (fetchCampaigns) as Service
import SweetSpot.Session (SessionId)
import SweetSpot.Session (getSessionId) as Session
import SweetSpot.Shopify as Shopify
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window) as HTML
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document) as Window

data RemoteResource a
  = Empty
  | Loading
  | Resource a
  | Error String

derive instance eqRemoteResource :: Eq a => Eq (RemoteResource a)

eitherToResource :: Either String ~> RemoteResource
eitherToResource = either Error Resource

mkApp :: Component { sessionId :: SessionId }
mkApp = do
  experimentListPage <- mkExperimentListPage
  experimentPage <- mkExperimentPage
  component "App" \props -> React.do
    route /\ setRoute <- useState (Campaign expensiveJacketsCheapMonkeysCampaign)
    let
      setRoute' = const >>> setRoute
    campaignsResource /\ setCampaignsResource <- useState Empty
    let
      setCampaignsResource' = const >>> setCampaignsResource
    useAff props.sessionId do
      liftEffect $ setCampaignsResource $ const Loading
      eCampaigns <- Service.fetchCampaigns props.sessionId
      eitherToResource eCampaigns
        # setCampaignsResource'
        >>> liftEffect
      Resource [ expensiveJacketsCheapMonkeysCampaign, storewide10Campaign ]
        # setCampaignsResource'
        >>> liftEffect
      pure unit
    let
      onViewCampaignByCampaign = Campaign >>> setRoute'
    pure
      $ element Shopify.appProvider
          { i18n: Shopify.enTranslations
          , children:
              case route of
                CampaignList -> case campaignsResource of
                  Empty -> R.text "EMPTY"
                  Loading -> R.text "LOADING"
                  Error err -> R.text ("ERROR: " <> err)
                  Resource campaigns ->
                    if Array.null campaigns then
                      gettingStartedPage
                    else
                      experimentListPage
                        { campaigns
                        , onViewCampaignByCampaign: onViewCampaignByCampaign
                        , onCreateExperiment: mempty
                        }
                Campaign campaign -> experimentPage { campaign }
          }

main :: Effect Unit
main = do
  mSessionId <- Session.getSessionId
  documentNode <- HTML.window >>= Window.document >>= toNonElementParentNode >>> pure
  mAppElement <- getElementById "app" documentNode
  app <- mkApp
  case mAppElement of
    Nothing -> throw "app element not found."
    Just appElement -> case mSessionId of
      Nothing -> render missingSessionPage appElement
      Just sessionId -> render (app { sessionId }) appElement
