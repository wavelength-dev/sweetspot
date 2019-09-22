{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module SweetSpot.Route.Injectable
  ( InjectableAPI
  , injectableHandler
  , UserBucketRoute
  ) where

import Control.Lens ((^.), (^?))
import Data.Aeson (Value)
import Data.Aeson.Lens (_String, key)
import Data.Text (Text)
import qualified Data.Text as T
import Servant
import SweetSpot.AppM (AppM(..), ServerM)
import SweetSpot.Data.Api
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Injectable
 ( InjectableDB(..)
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

getUserBucketsHandler :: Maybe Text -> Maybe Text -> ServerM [TestMap]
-- Existing user
getUserBucketsHandler mCmpId (Just uid) = runAppM $ do
  res <- getUserBuckets (UserId uid)
  case (mCmpId, res) of
    (_, buckets@(b:bs)) -> do
      L.info $ "Got " <> showNumber (length buckets) <> " bucket(s) for userId: " <> uid
      return (map createTestMap buckets)
    (Nothing, []) -> do
      L.info $ "Could not find bucket(s) for userId: " <> uid
      throwError notFoundErr
    (Just newCmpId, []) -> do
      let cId = CampaignId newCmpId
      isValidCampaign <- validateCampaign cId
      if isValidCampaign
        then do
            buckets <- getNewCampaignBuckets cId (Just (UserId uid))
            return $ map createTestMap buckets
        else do
          L.info $ "Got invalid campaign id for existing user" <> newCmpId
          throwError badRequestErr
-- New user
getUserBucketsHandler (Just cmpId) Nothing = runAppM $ do
  isValidCampaign <- validateCampaign (CampaignId cmpId)
  if isValidCampaign
    then do
      L.info $ "Got campaign " <> cmpId
      buckets <- getNewCampaignBuckets (CampaignId cmpId) Nothing
      return $ map createTestMap buckets
    else do
      L.info $ "Got invalid campaign id " <> cmpId
      throwError badRequestErr

getUserBucketsHandler Nothing Nothing = throwError badRequestErr

trackEventHandler :: Value -> ServerM OkResponse
trackEventHandler val = runAppM $ do
  let pageType = val ^? key "page" . _String
      step = val ^? key "step" . _String
      -- Relies on show instance of page in injectable
      input =
        case (pageType, step) of
          (Just "checkout", Just "thank_you") -> (Checkout, val)
          _ -> (View, val)
  insertEvent input
  L.info "Tracked event"
  return OkResponse {message = "Event received"}

trackLogMessageHandler :: Value -> ServerM OkResponse
trackLogMessageHandler val = runAppM $ do
  insertEvent (Log, val)
  return OkResponse {message = "Event received"}

injectableHandler =
  getUserBucketsHandler :<|> trackEventHandler :<|> trackLogMessageHandler
