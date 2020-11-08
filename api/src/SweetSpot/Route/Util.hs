module SweetSpot.Route.Util where

import RIO
import Servant
import SweetSpot.AppM
import SweetSpot.Data.Common
import SweetSpot.Env (Environment (..))

internalServerErr = err500 {errBody = "Something went wrong"}

badRequestErr = err400 {errBody = "Bad request"}

unauthorizedErr = err403 {errBody = "Unauthorized"}

notFoundErr = err404 {errBody = "Not found"}

getAppUrl :: ShopDomain -> AppM Text
getAppUrl domain = do
  env <- asks (view ctxConfig >>> view configEnvironment)
  let appPath = case env of
        Prod -> "/admin/apps/sweetspot"
        _ -> "/admin/apps/sweetspot-1"
  pure $ "https://" <> showText domain <> appPath

type Get303 (cts :: [*]) a =
  Verb 'GET 303 cts (Headers '[(Header "Location" Text)] a)
