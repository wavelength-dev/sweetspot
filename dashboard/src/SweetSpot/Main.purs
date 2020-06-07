module SweetSpot.Main where

import Prelude

import Data.Array (find)
import Data.Array (null) as Array
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (parallel, sequential) as Aff
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import React.Basic.DOM (text) as R
import React.Basic.Hooks (Component, component, element, empty, useEffectOnce, useState', (/\))
import React.Basic.Hooks (bind, discard) as React
import React.Basic.Hooks.Aff (useAff)
import SweetSpot.CampaignCreatePage (mkCampaignCreatePage)
import SweetSpot.CampaignListPage (mkCampaignListPage)
import SweetSpot.CampaignViewPage (mkCampaignViewPage)
import SweetSpot.Data.Api (UICampaign(..))
import SweetSpot.GettingStartedPage (gettingStartedPage)
import SweetSpot.Logger (logInfo) as Logger
import SweetSpot.MissingSessionPage (missingSessionPage)
import SweetSpot.Mock (bonoboHat, expensiveJacketsCheapMonkeysCampaign, storewide10Campaign)
import SweetSpot.Route (Route(..), useRouter)
import SweetSpot.Service (fetchCampaigns, fetchProducts) as Service
import SweetSpot.Session (SessionId)
import SweetSpot.Session (getSessionId) as Session
import SweetSpot.Shopify as Shopify
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window) as HTML
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Location (Location)
import Web.HTML.Location as Location
import Web.HTML.Window (document, location) as Window

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
  campaignListPage <- mkCampaignListPage
  campaignViewPage <- mkCampaignViewPage
  campaignCreatePage <- mkCampaignCreatePage
  component "App" \props -> React.do
    route /\ setRoute <- useState' CampaignList
    useEffectOnce (useRouter setRoute)
    campaignsResource /\ setCampaignsResource <- useState' Empty
    productsResource /\ setProductsResource <- useState' Empty
    useAff props.sessionId do
      liftEffect $ setCampaignsResource Loading
      eProducts /\ eCampaigns <-
        Aff.sequential
          $ Tuple
          <$> Aff.parallel (Service.fetchProducts props.sessionId)
          <*> Aff.parallel (Service.fetchCampaigns props.sessionId)
      eitherToResource eCampaigns
        # setCampaignsResource
        >>> liftEffect
      eitherToResource eProducts
        # setProductsResource
        >>> liftEffect
      pure unit
    pure
      $ element Shopify.appProvider
          { i18n: Shopify.enTranslations
          , children:
              [ case route of
                  CampaignList -> case campaignsResource of
                    Empty -> R.text "EMPTY"
                    Loading -> R.text "LOADING"
                    Error err -> R.text ("ERROR: " <> err)
                    Resource campaigns ->
                      if Array.null campaigns then
                        gettingStartedPage
                      else
                        campaignListPage { campaigns }
                  CampaignCreate -> case productsResource of
                    Empty -> R.text "EMPTY"
                    Loading -> R.text "LOADING"
                    Error err -> R.text ("ERROR: " <> err)
                    Resource products -> campaignCreatePage { products, sessionId: props.sessionId }
                  CampaignView rawCampaignId -> case campaignsResource of
                    Empty -> empty
                    Loading -> R.text "LOADING"
                    Error err -> R.text ("ERROR: " <> err)
                    Resource campaigns -> case find (getCampaignById rawCampaignId) campaigns of
                      Nothing -> R.text ("ERROR: campaign not found")
                      Just campaign -> campaignViewPage { campaign: campaign }
              ]
          }
  where
  getCampaignById targetId (UICampaign campaign) = targetId == campaign._uiCampaignId

main :: Effect Unit
main = do
  hostname <- HTML.window >>= Window.location >>= Location.hostname
  Logger.logInfo "Dashboard opened on " <> hostname
  mSessionId <- Session.getSessionId
  documentNode <- HTML.window >>= Window.document >>= toNonElementParentNode >>> pure
  mAppElement <- getElementById "app" documentNode
  app <- mkApp
  case mAppElement of
    Nothing -> throw "app element not found."
    Just appElement -> case mSessionId of
      Nothing -> render missingSessionPage appElement
      Just sessionId -> render (app { sessionId }) appElement
