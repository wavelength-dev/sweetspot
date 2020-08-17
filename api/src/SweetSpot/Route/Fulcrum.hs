module SweetSpot.Route.Fulcrum
  ( FulcrumAPI,
    fulcrumHandler,
    UserTestRoute,
  )
where

import RIO
import Servant
import SweetSpot.AppM (AppM (..), ServerM)
import SweetSpot.Data.Api
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Fulcrum (FulcrumDB (..))
import qualified SweetSpot.Logger as L

type UserTestRoute =
  "bucket" :> QueryParam' '[Required] "shop" ShopDomain
    :> QueryParam' '[Required] "uid" UserId
    :> Get '[JSON] [TestMap]

type UserCartTokenRoute =
  "cart-token" :> QueryParam' '[Required] "shop" ShopDomain
    :> ReqBody '[JSON] CartTokenReq
    :> Put '[JSON] OkResponse

type FulcrumAPI = "fulcrum" :> (UserTestRoute :<|> UserCartTokenRoute)

getUserTestHandler :: ShopDomain -> UserId -> ServerM [TestMap]
getUserTestHandler shopDomain uid = runAppM $ do
  res <- getUserTestMaps shopDomain uid
  case res of
    [] -> do
      L.info $ "Could not find bucket(s) for userId: " <> tshow uid
      pure []
    testMaps -> do
      L.info $ "Got " <> showText (length testMaps) <> " test maps(s) for userId: " <> tshow uid
      pure testMaps

userCartTokenHandler :: ShopDomain -> CartTokenReq -> ServerM OkResponse
userCartTokenHandler _ req = runAppM $ do
  L.info $ "Inserting user cart-token: " <> tshow req
  insertUserCartToken req
  L.info "Inserted token"
  return OkResponse {message = "Cart token received"}

fulcrumHandler =
  getUserTestHandler :<|> userCartTokenHandler
