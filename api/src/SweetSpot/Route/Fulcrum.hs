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
import SweetSpot.Route.Util

type UserTestRoute =
  "bucket" :> QueryParam' '[Required] "shop" ShopDomain
    :> QueryParam "sscid" CampaignId
    :> QueryParam' '[Required] "uid" UserId
    :> Get '[JSON] [TestMap]

type UserCartTokenRoute =
  "cart-token" :> QueryParam' '[Required] "shop" ShopDomain
    :> ReqBody '[JSON] CartTokenReq
    :> Put '[JSON] OkResponse

-- type LogEventRoute = "log" :> ReqBody '[ JSON] Value :> Post '[ JSON] OkResponse

type FulcrumAPI = "fulcrum" :> (UserTestRoute :<|> UserCartTokenRoute) -- :<|> LogEventRoute

-- originProtectedRoutes :: [Text]
-- originProtectedRoutes = ["bucket", "event", "log"]

getUserTestHandler :: ShopDomain -> Maybe CampaignId -> UserId -> ServerM [TestMap]
getUserTestHandler shopDomain mCmpId uid = runAppM $ do
  res <- getUserTestMaps shopDomain uid
  case (mCmpId, res) of
    (_, testMaps@(m : ms)) -> do
      L.info $ "Got " <> showText (length testMaps) <> " test maps(s) for userId: " <> tshow uid
      return testMaps
    (Nothing, []) -> do
      L.info $ "Could not find bucket(s) for userId: " <> tshow uid
      throwError notFoundErr
    (Just newCmpId, []) -> do
      isValidCampaign <- validateCampaign newCmpId
      if isValidCampaign
        then getNewCampaignTestMaps shopDomain newCmpId uid
        else do
          L.info $ "Got invalid campaign id for existing user: " <> showText newCmpId
          throwError notFoundErr

userCartTokenHandler :: ShopDomain -> CartTokenReq -> ServerM OkResponse
userCartTokenHandler _ req = runAppM $ do
  insertUserCartToken req
  return OkResponse {message = "Cart token received"}

-- trackLogMessageHandler :: Value -> ServerM OkResponse
-- trackLogMessageHandler val = runAppM $ do
--   insertEvent (Log, val)
--   return OkResponse {message = "Event received"}

fulcrumHandler =
  getUserTestHandler :<|> userCartTokenHandler
-- :<|> trackLogMessageHandler
