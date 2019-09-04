{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Route.Injectable
  ( InjectableAPI
  , injectableHandler
  , experimentShield
  , UserBucketRoute
  ) where

import Control.Lens ((^.), (^?))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (Value, encode)
import Data.Aeson.Lens (_String, key)
import Data.ByteString as BS (isInfixOf)
import Data.Pool (withResource)
import qualified Data.Text as T
import Data.Text (Text)
import Network.HTTP.Types (hContentType, status400)
import Network.Wai (Middleware, requestHeaders, responseLBS)
import Network.Wai.Middleware.Routed (routedMiddleware)
import Servant
import SweetSpot.AppM (AppConfig(..), AppCtx(..), AppM)
import SweetSpot.Data.Api
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Injectable
 (  getNewCampaignBuckets
  , getUserBuckets
  , validateCampaign
  , insertEvent
 )

import qualified SweetSpot.Logger as L
import SweetSpot.Route.Util (badRequestErr, notFoundErr)

type UserBucketRoute
   = "bucket" :> QueryParam "sscid" Text :> QueryParam "uid" Text :> Get '[ JSON] [TestMap]

type EventRoute = "event" :> ReqBody '[ JSON] Value :> Post '[ JSON] OkResponse

type LogEventRoute = "log" :> ReqBody '[ JSON] Value :> Post '[ JSON] OkResponse

type InjectableAPI = UserBucketRoute :<|> EventRoute :<|> LogEventRoute

originProtectedRoutes :: [Text]
originProtectedRoutes = ["bucket", "event", "log"]

showNumber :: Int -> Text
showNumber = T.pack . show

createTestMap :: UserBucket -> TestMap
createTestMap ub =
  case ub ^. ubBucketType of
    Control -> TestMap
      { userId = ub ^. ubUserId
      , targetId = ub ^. ubTestSvid
      , sku = ub ^. ubSku
      , swapId = ub ^. ubOriginalSvid
      , swapPrice = ub ^. ubControlPrice
      }
    Test -> TestMap
      { userId = ub ^. ubUserId
      , targetId = ub ^. ubOriginalSvid
      , sku = ub ^. ubSku
      , swapId = ub ^. ubTestSvid
      , swapPrice = ub ^. ubPrice
      }

getUserBucketsHandler :: Maybe Text -> Maybe Text -> AppM [TestMap]
-- Existing user
getUserBucketsHandler mCmpId (Just uid) = do
  pool <- asks _getDbPool
  res <- liftIO . withResource pool $ \conn -> getUserBuckets conn (UserId uid)
  case (mCmpId, res) of
    (_, buckets@(b:bs)) -> do
      L.info $ "Got " <> showNumber (length buckets) <> " bucket(s) for userId: " <> uid
      return (map createTestMap buckets)
    (Nothing, []) -> do
      L.info $ "Could not find bucket(s) for userId: " <> uid
      throwError notFoundErr
    (Just newCmpId, []) -> do
      let cId = CampaignId newCmpId
      isValidCampaign <- liftIO . withResource pool $ \conn -> validateCampaign conn cId
      if isValidCampaign
        then liftIO $ do
            buckets <- withResource pool $ \conn -> getNewCampaignBuckets conn cId (Just (UserId uid))
            return $ map createTestMap buckets
        else do
          L.info $ "Got invalid campaign id for existing user" <> newCmpId
          throwError badRequestErr
-- New user
getUserBucketsHandler (Just cmpId) Nothing = do
  pool <- asks _getDbPool
  isValidCampaign <- liftIO . withResource pool $ \conn -> validateCampaign conn (CampaignId cmpId)
  if isValidCampaign
    then do
      L.info $ "Got campaign " <> cmpId
      buckets <- liftIO $ withResource pool $ \conn -> getNewCampaignBuckets conn (CampaignId cmpId) Nothing
      return $ map createTestMap buckets
    else do
      L.info $ "Got invalid campaign id " <> cmpId
      throwError badRequestErr

getUserBucketsHandler Nothing Nothing = throwError badRequestErr

trackEventHandler :: Value -> AppM OkResponse
trackEventHandler val = do
  let pageType = val ^? key "page" . _String
      step = val ^? key "step" . _String
      -- Relies on show instance of page in injectable
      input =
        case (pageType, step) of
          (Just "checkout", Just "thank_you") -> (Checkout, val)
          _ -> (View, val)
  pool <- asks _getDbPool
  liftIO . withResource pool $ \conn -> insertEvent conn input
  L.info "Tracked event"
  return OkResponse {message = "Event received"}

trackLogMessageHandler :: Value -> AppM OkResponse
trackLogMessageHandler val = do
  pool <- asks _getDbPool
  liftIO . withResource pool $ \conn -> insertEvent conn (Log, val)
  return OkResponse {message = "Event received"}

experimentShield :: AppCtx -> Middleware
experimentShield ctx =
  routedMiddleware
    (\routes -> Prelude.any (`elem` routes) originProtectedRoutes)
    (originMiddleware ctx)

-- Drops requests that are not from Kamikoto
originMiddleware :: AppCtx -> Middleware
originMiddleware ctx app req respond =
  -- if not isOriginKamikoto && env /= "dev"
  if False
    then respond $
         responseLBS
           status400
           [(hContentType, "application/json")]
           (encode (OkResponse {message = "Referer needs to be kamikoto"}))
    else app req respond
  where
    env = environment $ _getConfig ctx
    headers = requestHeaders req
    isOriginKamikoto =
      case lookup "referer" headers of
        Nothing -> False
        Just referer -> "longvadon" `BS.isInfixOf` referer

injectableHandler =
  getUserBucketsHandler :<|> trackEventHandler :<|> trackLogMessageHandler
