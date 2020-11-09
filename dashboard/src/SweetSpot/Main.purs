module SweetSpot.Main where

import Prelude
import Data.Array (find)
import Data.Array (null) as Array
import Data.Lens ((^.))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (attempt) as Aff
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import React.Basic.DOM (css, div, text, p) as R
import React.Basic.DOM (render)
import React.Basic.Hooks (Component, component, element, useEffectAlways, useEffectOnce, useState', (/\))
import React.Basic.Hooks (bind, discard) as React
import React.Basic.Hooks.Aff (useAff)
import SweetSpot.CampaignCreatePage (mkCampaignCreatePage)
import SweetSpot.CampaignListPage (mkCampaignListPage)
import SweetSpot.CampaignViewPage (mkCampaignViewPage)
import SweetSpot.Data.Api (UICampaign(..), AppChargeResponse(..), status, confirmationUrl)
import SweetSpot.Mock as Mock
import SweetSpot.GettingStartedPage (gettingStartedPage)
import SweetSpot.Logger (LogLevel(..)) as LogLevel
import SweetSpot.Logger (log, logWithContext) as Logger
import SweetSpot.MissingSessionPage (missingSessionPage)
import SweetSpot.Route (Route(..), useRouter)
import SweetSpot.Service (fetchCampaigns, fetchAppCharge) as Service
import SweetSpot.Session (SessionId)
import SweetSpot.Session (getSessionId) as Session
import SweetSpot.Shopify as Shopify
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window) as HTML
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Location as Location
import Web.HTML.Window (document, history, location) as Window

data RemoteResource a
  = EmptyResource
  | Loading
  | Resource a
  | FetchFail

derive instance eqRemoteResource :: Eq a => Eq (RemoteResource a)

ensureRootHash :: Effect (Effect Unit)
ensureRootHash = do
  window <- HTML.window
  currentHash <- HTML.window >>= Window.location >>= Location.hash
  history <- HTML.window >>= Window.history
  when (currentHash == "") do
    HTML.window >>= Window.location >>= Location.setHash "#/"
  pure mempty

mkApp :: Component { sessionId :: SessionId }
mkApp = do
  campaignListPage <- mkCampaignListPage
  campaignViewPage <- mkCampaignViewPage
  campaignCreatePage <- mkCampaignCreatePage
  component "App" \props -> React.do
    useEffectAlways ensureRootHash
    route /\ setRoute <- useState' CampaignList
    useEffectOnce (useRouter setRoute)
    campaignsResource /\ setCampaignsResource <- useState' EmptyResource
    appChargeResource /\ setAppChargeResource <- useState' EmptyResource
    useAff props.sessionId do
      setCampaignsResource Loading # liftEffect
      setAppChargeResource Loading # liftEffect
      eCampaigns <- Service.fetchCampaigns props.sessionId # Aff.attempt
      eAppCharge <- Service.fetchAppCharge props.sessionId # Aff.attempt
      liftEffect case eCampaigns, eAppCharge of
        Right campaigns, Right appCharge -> do
          setCampaignsResource (Resource campaigns)
          setAppChargeResource (Resource appCharge)
        Left error, Right _ -> do
          setCampaignsResource FetchFail
          Logger.logWithContext LogLevel.Error "failed to fetch campaigns" error
        Right _, Left error -> do
          setAppChargeResource FetchFail
          Logger.logWithContext LogLevel.Error "failed to fetch appCharge" error
        Left err0, Left err1 -> do
          setCampaignsResource FetchFail
          setAppChargeResource FetchFail
          Logger.logWithContext LogLevel.Error "failed to fetch campaigns" err0
          Logger.logWithContext LogLevel.Error "failed to fetch appCharge" err1
      pure unit
    pure
      $ element Shopify.appProvider
          { i18n: Shopify.enTranslations
          , children:
              [ case appChargeResource of
                  Resource appCharge -> case appCharge ^. status of
                    "active" -> mempty
                    _ ->
                      element
                        Shopify.banner
                        { title: "SweetSpot is in evaluation mode"
                        , action: { content: "Upgrade now", url: appCharge ^. confirmationUrl }
                        , status: "info"
                        , onDismiss: mempty
                        , children:
                            [ R.p
                                { children:
                                    [ R.text "You're currently using the free evaluation version. If you want to run your first experiment, please upgrade your plan." ]
                                }
                            ]
                        }
                  _ -> mempty
              , case route of
                  CampaignList -> case campaignsResource of
                    EmptyResource -> mempty
                    Loading -> loading
                    FetchFail -> R.text ("ERROR: failed to fetch")
                    Resource campaigns ->
                      if Array.null campaigns then
                        gettingStartedPage
                      else
                        campaignListPage { campaigns }
                  CampaignCreate ->
                    campaignCreatePage
                      { sessionId: props.sessionId
                      , enableCreate:
                          case appChargeResource of
                            Resource appCharge -> appCharge ^. status == "active"
                            _ -> false
                      }
                  CampaignView rawCampaignId -> case campaignsResource of
                    EmptyResource -> mempty
                    Loading -> loading
                    FetchFail -> R.text ("ERROR: failed to fetch")
                    Resource campaigns -> case find (getCampaignById rawCampaignId) campaigns of
                      Nothing -> R.text "ERROR: campaign not found"
                      Just campaign -> campaignViewPage { campaign: campaign, sessionId: props.sessionId }
              ]
          }
  where
  loading =
    R.div
      { style: R.css { height: "100px" }
      , children:
          [ element Shopify.frame
              { children: [ element Shopify.loading {} ] }
          ]
      }

  getCampaignById targetId (UICampaign campaign) = targetId == campaign._uiCampaignId

main :: Effect Unit
main = do
  hostname <- HTML.window >>= Window.location >>= Location.hostname
  Logger.log LogLevel.Info $ "Dashboard opened on " <> hostname
  mSessionId <- Session.getSessionId
  documentNode <- HTML.window >>= Window.document >>= toNonElementParentNode >>> pure
  mAppElement <- getElementById "app" documentNode
  app <- mkApp
  case mAppElement of
    Nothing -> throw "app element not found."
    Just appElement -> case mSessionId of
      Nothing -> render missingSessionPage appElement
      Just sessionId -> render (app { sessionId }) appElement
