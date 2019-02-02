{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Supple.Route.Injectable
  ( InjectableAPI
  , injectableHandler
  ) where

import Control.Applicative ((*>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Time.Clock (getCurrentTime)
import Servant
import Supple.AppM (AppCtx(..), AppM, LogMessage(..))
import Supple.Database (getNewUserBuckets, getUserBuckets, insertEvent)
import Supple.Types
import System.Log.FastLogger (ToLogStr(..), pushLogStrLn)

type UserBucketRoute
   = "bucket" :> QueryParam "uid" Int :> Get '[ JSON] [UserBucket]

type EventRoute
   = "event" :> ReqBody '[ JSON] TrackView :> Post '[ JSON] OkResponse

type InjectableAPI = UserBucketRoute :<|> EventRoute

getUserBucketHandler :: Maybe Int -> AppM [UserBucket]
getUserBucketHandler (Just uid) = do
  dbconn <- asks _getDbConn
  logset <- asks _getLogger
  ts <- liftIO getCurrentTime
  res <- liftIO $ getUserBuckets dbconn uid
  liftIO $
    pushLogStrLn logset $
    toLogStr LogMessage {logMessage = "Got user bucket", timestamp = ts}
  return res
getUserBucketHandler Nothing = do
  dbconn <- asks _getDbConn
  res <- liftIO $ getNewUserBuckets dbconn
  return res

trackViewHandler :: TrackView -> AppM OkResponse
trackViewHandler tv = do
  dbconn <- asks _getDbConn
  liftIO $
    insertEvent dbconn tv *> return OkResponse {message = "Event received"}

injectableHandler = getUserBucketHandler :<|> trackViewHandler
