module SweetSpot.Route.AppCharge
  ( AppChargeAPI,
    ActivateAppChargeRoute,
    appChargeHandler,
  )
where

import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import RIO
import Servant
import SweetSpot.AppM
import SweetSpot.Data.Api (AppChargeStatusResponse (..))
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Dashboard (DashboardDB (..))
import SweetSpot.Database.Queries.Install (InstallDB (..))
import SweetSpot.Database.Schema
import qualified SweetSpot.Logger as L
import SweetSpot.Route.Util
import SweetSpot.Shopify.Client (MonadShopify (..))

type ActivateAppChargeRoute =
  "charge" :> "activate"
    :> QueryParam' '[Required, Strict] "shop" ShopDomain
    :> Get303 '[PlainText] NoContent

type AppChargeStatusRoute =
  "charge" :> "status"
    :> QueryParam' '[Required, Strict] "session" SessionId
    :> Get '[JSON] AppChargeStatusResponse

type AppChargeAPI = ActivateAppChargeRoute :<|> AppChargeStatusRoute

activateAppChargeHandler ::
  ShopDomain ->
  ServerM (Headers '[Header "Location" Text] NoContent)
activateAppChargeHandler domain = runAppM $ do
  charge <- getAppCharge domain
  let chargeId = charge ^. appChargeShopifyId
  result <- runExceptT $ do
    status <- ExceptT $ fetchAppChargeStatus domain chargeId
    case status of
      Accepted -> do
        updatedStatus <- ExceptT $ activateAppCharge domain chargeId
        lift $ updateAppChargeStatus chargeId updatedStatus
        pure $ Right ()
      _ -> pure $ Left "App charge not accepted"
  case result of
    Left err -> do
      L.error $
        "Failed to activate app charge for shop "
          <> showText domain
          <> " "
          <> err
      throwError internalServerErr
    Right _ -> do
      L.info $ "Successfully activated app charge for shop " <> showText domain
      appUrl <- getAppUrl domain
      pure $ addHeader appUrl NoContent

appChargeStatusRoute :: SessionId -> ServerM AppChargeStatusResponse
appChargeStatusRoute sessionId = runAppM $ do
  mDomain <- validateSessionId sessionId
  case mDomain of
    Nothing -> throwError unauthorizedErr
    Just domain -> do
      appCharge <- getAppCharge domain
      pure
        AppChargeStatusResponse
          { _appChargeStatusResponse = _appChargeStatus appCharge
          }

appChargeHandler = activateAppChargeHandler :<|> appChargeStatusRoute
