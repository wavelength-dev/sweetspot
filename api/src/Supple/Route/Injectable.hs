{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Supple.Route.Injectable
  ( InjectableAPI
  , injectableHandler
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text as T
import Servant
import Supple.AppM (AppCtx(..), AppM)
import Supple.Data.Api (OkResponse(..), TrackView, UserBucket)
import Supple.Data.Common
import Supple.Database (getNewUserBuckets, getUserBuckets, insertEvent)
import Supple.Route.Util (internalServerErr)
import qualified Supple.Logger as L

type UserBucketRoute
   = "bucket" :> QueryParam "uid" Int :> Get '[ JSON] [UserBucket]

type EventRoute
   = "event" :> ReqBody '[ JSON] TrackView :> Post '[ JSON] OkResponse

type InjectableAPI = UserBucketRoute :<|> EventRoute

getUserBucketHandler :: Maybe Int -> AppM [UserBucket]
getUserBucketHandler (Just uid) = do
  dbconn <- asks _getDbConn
  res <- liftIO $ getUserBuckets dbconn (UserId uid)
  case res of
    Right res -> do
      L.log $ "Got bucket for userId: " <> (T.pack $ show uid)
      return res
    Left err -> do
      L.log $ "Error getting user bucket: " <> err
      throwError internalServerErr
getUserBucketHandler Nothing = do
  dbconn <- asks _getDbConn
  res <- liftIO $ getNewUserBuckets dbconn
  case res of
    Right res -> L.log "Assigned user to bucket" >> return res
    Left err -> do
      L.log $ "Error assigning user to bucket " <> err
      throwError internalServerErr

trackViewHandler :: TrackView -> AppM OkResponse
trackViewHandler tv = do
  dbconn <- asks _getDbConn
  res <- liftIO $ insertEvent dbconn tv
  case res of
    Right _ ->
      L.log "Tracked view" >> return OkResponse {message = "Event received"}
    Left err -> do
      L.log $ "Error tracking view " <> err
      throwError internalServerErr

injectableHandler = getUserBucketHandler :<|> trackViewHandler
