{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module SweetSpot.Route.Injectable
  ( InjectableAPI
  , injectableHandler
  , UserTestRoute
  )
where

import Control.Lens ((^.), (^?))
import Data.Aeson (Value)
import Data.Aeson.Lens (_String, key)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID.Types (UUID)
import Servant
import SweetSpot.AppM (AppM(..), ServerM)
import SweetSpot.Data.Api
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Injectable (InjectableDB(..))

import qualified SweetSpot.Logger as L
import SweetSpot.Route.Util (badRequestErr, notFoundErr)

type UserTestRoute
   = "bucket" :> QueryParam "sscid" UUID :> QueryParam "uid" UUID :> Get '[ JSON] [TestMap]

type CheckoutEventRoute = "checkout" :> ReqBody '[ JSON] ApiCheckoutEvent :> Post '[ JSON] OkResponse

-- type LogEventRoute = "log" :> ReqBody '[ JSON] Value :> Post '[ JSON] OkResponse

type InjectableAPI = UserTestRoute  :<|> CheckoutEventRoute -- :<|> LogEventRoute

-- originProtectedRoutes :: [Text]
-- originProtectedRoutes = ["bucket", "event", "log"]

showNumber :: Int -> Text
showNumber = T.pack . show

showUuid :: UUID -> Text
showUuid = T.pack . show

getUserTestHandler :: Maybe UUID -> Maybe UUID -> ServerM [TestMap]
-- Existing user
getUserTestHandler mCmpId (Just uid) = runAppM $ do
  res <- getUserTestMaps (UserId uid)
  case (mCmpId, res) of
    (_, testMaps@(m:ms)) -> do
      L.info $ "Got " <> showNumber (length testMaps) <> " test maps(s) for userId: " <> showUuid uid
      return testMaps
    (Nothing, []) -> do
      L.info $ "Could not find bucket(s) for userId: " <> showUuid uid
      throwError notFoundErr
    (Just newCmpId, []) -> do
      let cId = CampaignId newCmpId
      isValidCampaign <- validateCampaign cId
      if isValidCampaign
        then
            getNewCampaignTestMaps cId (Just (UserId uid))
        else do
          L.info $ "Got invalid campaign id for existing user: " <> showUuid newCmpId
          throwError badRequestErr
-- New user
getUserTestHandler (Just cmpId) Nothing = runAppM $ do
  isValidCampaign <- validateCampaign (CampaignId cmpId)
  if isValidCampaign
    then do
      L.info $ "Got campaign " <> showUuid cmpId
      getNewCampaignTestMaps (CampaignId cmpId) Nothing
    else do
      L.info $ "Got invalid campaign id " <> showUuid cmpId
      throwError badRequestErr

getUserTestHandler Nothing Nothing = throwError badRequestErr

trackCheckoutEventHandler :: ApiCheckoutEvent -> ServerM OkResponse
trackCheckoutEventHandler event = runAppM $ do
  insertCheckoutEvent event
  return OkResponse {message = "Event received"}

-- trackLogMessageHandler :: Value -> ServerM OkResponse
-- trackLogMessageHandler val = runAppM $ do
--   insertEvent (Log, val)
--   return OkResponse {message = "Event received"}

injectableHandler =
  getUserTestHandler :<|> trackCheckoutEventHandler -- :<|> trackLogMessageHandler
