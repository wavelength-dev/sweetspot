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
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Install (InstallDB (..))
import SweetSpot.Database.Schema
import qualified SweetSpot.Logger as L
import SweetSpot.Route.Util
import SweetSpot.Shopify.Client (MonadShopify (..))
import SweetSpot.Shopify.Types

type ActivateAppChargeRoute =
  "charge" :> "activate"
    :> QueryParam' '[Required, Strict] "shop" ShopDomain
    :> Get303 '[PlainText] NoContent

type AppChargeAPI = ActivateAppChargeRoute

activateAppChargeHandler ::
  ShopDomain ->
  ServerM (Headers '[Header "Location" Text] NoContent)
activateAppChargeHandler domain = runAppM $ do
  charge <- getAppCharge domain
  let chargeId = charge ^. appChargeShopifyId
  result <- runExceptT $ do
    charge <- ExceptT $ fetchAppCharge domain chargeId
    case charge ^. createAppChargeResStatus of
      Accepted -> do
        updated <- ExceptT $ activateAppCharge domain chargeId
        lift $ updateAppChargeStatus updated
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
      pure $ addHeader ("https://" <> showText domain <> "/admin/apps/sweetspot") NoContent

appChargeHandler = activateAppChargeHandler
