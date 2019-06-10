{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Middleware
  ( getMiddleware
  ) where

import Network.Wai (Middleware)
import Network.Wai.Middleware.Gzip
  ( GzipFiles(GzipCacheFolder)
  , def
  , gzip
  , gzipFiles
  )
import Network.Wai.Middleware.HttpAuth (basicAuth)
import Network.Wai.Middleware.Routed (routedMiddleware)
import SweetSpot.AppM (AppCtx)
import SweetSpot.Route.Injectable (experimentShield)

-- WAI doesn't seem to want to know about routing.
-- This should probably move into a Servant handler somehow.
gzipStatic :: Middleware
gzipStatic = routedMiddleware ("static" `elem`) (gzip settings)
  where
    settings = def {gzipFiles = GzipCacheFolder "../dist/"}

auth :: Middleware
auth = routedMiddleware ("dashboard" `elem`) mw
  where
    check u p = return $ u == "sweetspot" && p == "***REMOVED***"
    mw = basicAuth check "Dashboard realm"

getMiddleware :: AppCtx -> Middleware
getMiddleware ctx = gzipStatic . auth . experimentShield ctx
