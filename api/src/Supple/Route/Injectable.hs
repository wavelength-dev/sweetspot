{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Supple.Route.Injectable
  ( InjectableAPI
  , injectableHandler
  , experimentShield
  ) where

import Control.Lens ((^?))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (Value, encode)
import Data.Aeson.Lens (_String, key)
import Data.ByteString as BS (isInfixOf)
import Data.Text as T
import Network.HTTP.Types (hContentType, status400)
import Network.Wai (Middleware, requestHeaders, responseLBS)
import Network.Wai.Middleware.Routed (routedMiddleware)
import Servant
import Supple.AppM (AppConfig(..), AppCtx(..), AppM)
import Supple.Data.Api (OkResponse(..), UserBucket)
import Supple.Data.Common
import Supple.Database
  ( getNewUserBucket
  , getUserBucket
  , insertEvent
  , insertLogEvent
  )
import qualified Supple.Logger as L
import Supple.Route.Util (internalServerErr)

type UserBucketRoute
   = "bucket" :> QueryParam "uid" Int :> Get '[ JSON] UserBucket

type EventRoute = "event" :> ReqBody '[ JSON] Value :> Post '[ JSON] OkResponse

type LogEventRoute = "log" :> ReqBody '[ JSON] Value :> Post '[ JSON] OkResponse

type InjectableAPI = UserBucketRoute :<|> EventRoute :<|> LogEventRoute

originProtectedRoutes :: [Text]
originProtectedRoutes = ["bucket", "event", "log"]

getUserBucketHandler :: Maybe Int -> AppM UserBucket
getUserBucketHandler (Just uid) = do
  pool <- asks _getDbPool
  res <- liftIO $ getUserBucket pool (UserId uid)
  case res of
    Right body -> do
      L.info $ "Got bucket for userId: " <> T.pack (show uid)
      return body
    Left err -> do
      L.error $ "Error getting user bucket: " <> err
      throwError internalServerErr
getUserBucketHandler Nothing = do
  pool <- asks _getDbPool
  res <- liftIO $ getNewUserBucket pool
  case res of
    Right body -> do
      L.info "Assigned user to bucket"
      return body
    Left err -> do
      L.error $ "Error assigning user to bucket " <> err
      throwError internalServerErr

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
  res <- liftIO $ insertEvent pool input
  case res of
    Right _ ->
      L.info "Tracked event" >> return OkResponse {message = "Event received"}
    Left err -> do
      L.error $ "Error tracking event " <> err
      throwError internalServerErr

trackLogMessageHandler :: Value -> AppM OkResponse
trackLogMessageHandler val = do
  pool <- asks _getDbPool
  res <- liftIO $ insertLogEvent pool val
  case res of
    Right _ ->
      L.info "Tracked log event" >>
      return OkResponse {message = "Event received"}
    Left err -> do
      L.error $ "Error tracking log event " <> err
      throwError internalServerErr

experimentShield :: AppCtx -> Middleware
experimentShield ctx =
  routedMiddleware
    (\routes -> Prelude.any (`elem` routes) originProtectedRoutes)
    (originMiddleware ctx)

-- Drops requests that are not from Kamikoto
originMiddleware :: AppCtx -> Middleware
originMiddleware ctx app req respond =
  if not isOriginKamikoto && env /= "dev"
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
        Just referer -> "kamikoto" `BS.isInfixOf` referer

injectableHandler ::
     (Maybe Int -> AppM UserBucket) :<|> ((Value -> AppM OkResponse) :<|> (Value -> AppM OkResponse))
injectableHandler =
  getUserBucketHandler :<|> trackEventHandler :<|> trackLogMessageHandler
