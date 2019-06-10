{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Route.Static
  ( StaticAPI
  , staticHandler
  ) where

import Servant
import WaiAppStatic.Types
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)

type StaticAPI = "static" :> Raw

-- staticHandler = serveDirectoryWebApp "../dist/"
staticHandler = serveDirectoryWith defaultOptions { ssMaxAge = MaxAgeSeconds 600 }
  where defaultOptions = defaultWebAppSettings "../dist"
