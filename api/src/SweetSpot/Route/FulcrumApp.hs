module SweetSpot.Route.FulcrumApp where

import Servant
  ( (:>),
    Raw,
    serveDirectoryWith,
  )
import WaiAppStatic.Storage.Filesystem
  ( defaultWebAppSettings,
  )
import WaiAppStatic.Types
  ( MaxAge (..),
    ssMaxAge,
  )

type FulcrumApp = "fulcrum" :> Raw

fulcrumAppHandler =
  serveDirectoryWith
    defaultOptions
      { ssMaxAge = MaxAgeSeconds 600
      }
  where
    defaultOptions = defaultWebAppSettings "./dist/fulcrum"
