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
import SweetSpot.Route.Util (badRequestErr)
import SweetSpot.Shopify.Types

type UserTestRoute =
  "bucket" :> QueryParam' '[Required] "shop" ShopDomain
    :> QueryParam' '[Required] "uid" UserId
    :> Get '[JSON] [TestMap]

type UserCheckoutRoute =
  "checkout" :> QueryParam' '[Required] "shop" ShopDomain
    :> ReqBody '[JSON] CheckoutPayload
    :> Post '[JSON] OkResponse

type FulcrumAPI =
  "fulcrum"
    :> ( UserTestRoute
           :<|> UserCheckoutRoute
       )

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

userCheckoutHandler :: ShopDomain -> CheckoutPayload -> ServerM OkResponse
userCheckoutHandler domain payload = runAppM $ do
  let uid = payload ^. checkoutPayloadUserId
      order = payload ^. checkoutPayloadOrder
  mShopId <- validateShopDomain domain
  mCampaignId <- getUserActiveCampaign domain uid
  exists <- orderExists (order ^. orderId)
  case (mShopId, mCampaignId, exists) of
    (Just sid, Just cid, False) -> do
      insertOrder sid cid uid order
      L.info $ "Received checkout for " <> tshow domain <> " campaign " <> tshow cid
      pure OkResponse {message = "Checkout received"}
    (_, _, True) -> pure OkResponse {message = "Checkout received"}
    _ -> do
      L.error $ "Unable to register checkout for " <> tshow domain <> ", user " <> tshow uid
      pure OkResponse {message = "Checkout received"}

fulcrumHandler =
  getUserTestHandler :<|> userCheckoutHandler
