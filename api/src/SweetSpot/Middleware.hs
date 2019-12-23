{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Middleware
  ( getMiddleware
  ) where


import Crypto.Hash (SHA256, Digest)
import Crypto.MAC.HMAC
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Types (status400, hContentType)
import Network.Wai ( Middleware
                   , queryString
                   , Application
                   , responseLBS
                   )
import Network.Wai.Middleware.Gzip ( GzipFiles(GzipCacheFolder)
                                   , def
                                   , gzip
                                   , gzipFiles
                                   )
import Network.Wai.Middleware.HttpAuth (basicAuth)
import Network.Wai.Middleware.Routed (routedMiddleware)
import SweetSpot.AppM (AppConfig(..), AppCtx(..))
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Injectable (validateDomain)
import SweetSpot.Database.Queries.Util (withConnIO)
import SweetSpot.Env (Environment(..))
import qualified SweetSpot.Logger as L

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
    else gzipStatic . auth user pass . validateShopDomain ctx . verifyHmac ctx
  where
    config = _getConfig ctx
    env = environment config
    user = encodeUtf8 $ basicAuthUser config
    pass = encodeUtf8 $ basicAuthPassword config

send400 :: BSL.ByteString -> Application
send400 msg _req sendResponse = sendResponse $ responseLBS
  status400 [(hContentType, "text/plain")] msg

verifyHmac :: AppCtx -> Middleware
verifyHmac ctx app req sendResponse =
  case (== digestTxt) <$> mSupplied of
    Just True -> app req sendResponse
    _ -> do
      let appLogger = _getLogger ctx
      L.warn' appLogger "Got invalid hmac digest"
      send400 "Invalid HMAC digest" req sendResponse
  where
    secret = shopifyClientSecret . _getConfig $ ctx
    params = queryString req
    mSupplied = decodeUtf8 <$> (L.find ((== "hmac") . fst) params >>= snd)
    sansHMAC = filter ((/= "hmac") . fst) params
    joined = mapMaybe (\(key, val) -> fmap (\v -> key <> "=" <> v) val) sansHMAC
    checkable = BS.intercalate "&" joined
    digest = hmacGetDigest $ hmac (encodeUtf8 secret) checkable :: Digest SHA256
    digestTxt = T.pack $ show digest

validateShopDomain :: AppCtx -> Middleware
validateShopDomain ctx app req sendResponse = do
  let
    pool = _getDbPool ctx
    appLogger = _getLogger ctx
    params = queryString req
    mSuppliedDomain =
      ShopDomain . decodeUtf8 <$> (snd =<< L.find ((== "shop") . fst) params)
  case mSuppliedDomain of
    Just domain -> do
      mShopDomain <- withConnIO pool $ \conn -> validateDomain conn domain
      case mShopDomain of
        Just _ -> app req sendResponse
        Nothing -> do
          L.warn' appLogger $ "Got invalid shop query parameter: " <> showText domain
          send400 "Got invalid shop query parameter" req sendResponse
    _ -> do
      L.warn' appLogger "Missing shop query parameter"
      send400 "Missing shop query parameter" req sendResponse
