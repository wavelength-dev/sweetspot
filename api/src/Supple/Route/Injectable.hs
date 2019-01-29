{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Supple.Route.Injectable
  ( InjectableAPI
  , injectableHandler
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Time.Clock (getCurrentTime)
import Servant
import Supple.AppM (AppCtx(..), AppM, LogMessage(..))
import Supple.Database (getNewUserBuckets, getUserBuckets)
import Supple.Types
import System.Log.FastLogger (ToLogStr(..), pushLogStrLn)

type UserBucketRoute
   = "bucket" :> QueryParam "uid" Int :> Get '[ JSON] [UserBucket]

type InjectableAPI = UserBucketRoute

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

injectableHandler = getUserBucketHandler
