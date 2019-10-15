{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Middleware
  ( getMiddleware
  ) where

import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Gzip
  ( GzipFiles(GzipCacheFolder)
  , def
  , gzip
  , gzipFiles
  )
import Network.Wai.Middleware.HttpAuth (basicAuth)
import Network.Wai.Middleware.Routed (routedMiddleware)
import SweetSpot.AppM (AppConfig(..), AppCtx(..))
import SweetSpot.Env (Environment(..))

-- WAI doesn't seem to want to know about routing.
-- This should probably move into a Servant handler somehow.
gzipStatic :: Middleware
gzipStatic = routedMiddleware ("static" `elem`) (gzip settings)
  where
    settings = def {gzipFiles = GzipCacheFolder "../dist/"}

auth :: ByteString -> ByteString -> Middleware
auth user pass = routedMiddleware ("dashboard" `elem`) mw
  where
    check u p = return $ u == user && p == pass
    mw = basicAuth check "Dashboard realm"

getMiddleware :: AppCtx -> Middleware
getMiddleware ctx =
  -- Disable auth in dev for ease of testing
  if env == Dev
    then gzipStatic
    else gzipStatic . auth user pass
  where
    env = environment . _getConfig $ ctx
    user = encodeUtf8 $ basicAuthUser . _getConfig $ ctx
    pass = encodeUtf8 $ basicAuthPassword . _getConfig $ ctx
