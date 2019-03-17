{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Supple.Route.Injectable
  ( InjectableAPI
  , injectableHandler
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (Value)
import Data.Text as T
import Servant
import Supple.AppM (AppCtx(..), AppM)
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

type EventRoute
   = "event" :> ReqBody '[ JSON] Value :> Post '[ JSON] OkResponse

type LogEventRoute = "log" :> ReqBody '[ JSON] Value :> Post '[ JSON] OkResponse

type InjectableAPI = UserBucketRoute :<|> EventRoute :<|> LogEventRoute

getUserBucketHandler :: Maybe Int -> AppM UserBucket
getUserBucketHandler (Just uid) = do
  pool <- asks _getDbPool
  res <- liftIO $ getUserBucket pool (UserId uid)
  case res of
    Right res -> do
      L.info $ "Got bucket for userId: " <> (T.pack $ show uid)
      return res
    Left err -> do
      L.error $ "Error getting user bucket: " <> err
      throwError internalServerErr
getUserBucketHandler Nothing = do
  pool <- asks _getDbPool
  res <- liftIO $ getNewUserBucket pool
  case res of
    Right res -> L.info "Assigned user to bucket" >> return res
    Left err -> do
      L.error $ "Error assigning user to bucket " <> err
      throwError internalServerErr

trackViewHandler :: Value -> AppM OkResponse
trackViewHandler val = do
  pool <- asks _getDbPool
  res <- liftIO $ insertEvent pool val
  case res of
    Right _ ->
      L.info "Tracked view" >> return OkResponse {message = "Event received"}
    Left err -> do
      L.error $ "Error tracking view " <> err
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

injectableHandler =
  getUserBucketHandler :<|> trackViewHandler :<|> trackLogMessageHandler
