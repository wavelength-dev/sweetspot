module SweetSpot.Route.Fulcrum
  ( FulcrumAPI,
    fulcrumHandler,
    UserTestRoute,
  )
where

import Servant
import SweetSpot.AppM (AppM (..), ServerM)
import SweetSpot.Data.Api
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Fulcrum (FulcrumDB (..))
import qualified SweetSpot.Logger as L
import SweetSpot.Route.Util

type UserTestRoute =
  "bucket" :> QueryParam "shop" ShopDomain
    :> QueryParam "sscid" CampaignId
    :> QueryParam "uid" UserId
    :> Get '[JSON] [TestMap]

type CheckoutEventRoute =
  "checkout" :> QueryParam "shop" ShopDomain
    :> ReqBody '[JSON] ApiCheckoutEvent
    :> Post '[JSON] OkResponse

type UserCartTokenRoute =
  "cart-token" :> QueryParam "shop" ShopDomain
    :> ReqBody '[JSON] CartTokenReq
    :> Put '[JSON] OkResponse

-- type LogEventRoute = "log" :> ReqBody '[ JSON] Value :> Post '[ JSON] OkResponse

type FulcrumAPI = "fulcrum" :> (UserTestRoute :<|> CheckoutEventRoute :<|> UserCartTokenRoute) -- :<|> LogEventRoute

-- originProtectedRoutes :: [Text]
-- originProtectedRoutes = ["bucket", "event", "log"]

getUserTestHandler :: Maybe ShopDomain -> Maybe CampaignId -> Maybe UserId -> ServerM [TestMap]
-- Existing user
getUserTestHandler (Just shopDomain) mCmpId (Just uid) = runAppM $ do
  res <- getUserTestMaps uid
  case (mCmpId, res) of
    (_, testMaps@(m : ms)) -> do
      L.info $ "Got " <> showText (length testMaps) <> " test maps(s) for userId: " <> showText uid
      return testMaps
    (Nothing, []) -> do
      L.info $ "Could not find bucket(s) for userId: " <> showText uid
      throwError notFoundErr
    (Just newCmpId, []) -> do
      isValidCampaign <- validateCampaign newCmpId
      if isValidCampaign
        then getNewCampaignTestMaps newCmpId (Just uid)
        else do
          L.info $ "Got invalid campaign id for existing user: " <> showText newCmpId
          throwError notFoundErr
-- New user
getUserTestHandler (Just shopDomain) (Just cmpId) Nothing = runAppM $ do
  isValidCampaign <- validateCampaign cmpId
  if isValidCampaign
    then do
      L.info $ "Got campaign " <> showText cmpId
      getNewCampaignTestMaps cmpId Nothing
    else do
      L.info $ "Got invalid campaign id for new user: " <> showText cmpId
      throwError notFoundErr
getUserTestHandler _ _ _ = throwError badRequestErr

trackCheckoutEventHandler :: Maybe ShopDomain -> ApiCheckoutEvent -> ServerM OkResponse
trackCheckoutEventHandler (Just shopDomain) event = runAppM $ do
  mShopId <- validateShopDomain shopDomain
  case mShopId of
    Just shopId -> do
      insertCheckoutEvent shopId event
      return OkResponse {message = "Event received"}
    Nothing -> do
      L.error $ "Got invalid shopDomain: " <> showText shopDomain <> ", cannot track checkout event"
      throwError badRequestErr
trackCheckoutEventHandler Nothing _ = runAppM $ do
  L.error "Missing shopDomain, cannot track checkout event"
  throwError badRequestErr

userCartTokenHandler :: Maybe ShopDomain -> CartTokenReq -> ServerM OkResponse
userCartTokenHandler (Just _) req = runAppM $ do
  insertUserCartToken req
  return OkResponse {message = "Cart token received"}
userCartTokenHandler Nothing _ = runAppM $ do
  L.error "Missing shopDomain, cannot insert cart token"
  throwError badRequestErr

-- trackLogMessageHandler :: Value -> ServerM OkResponse
-- trackLogMessageHandler val = runAppM $ do
--   insertEvent (Log, val)
--   return OkResponse {message = "Event received"}

fulcrumHandler =
  getUserTestHandler :<|> trackCheckoutEventHandler :<|> userCartTokenHandler
-- :<|> trackLogMessageHandler
