module SweetSpot.Middleware
  ( getMiddleware,
  )
where

import Crypto.Hash (Digest, SHA256)
import Crypto.MAC.HMAC
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Network.HTTP.Types
  ( hContentType,
    hLocation,
    status302,
    status400,
  )
import Network.Wai
  ( Application,
    Middleware,
    queryString,
    rawQueryString,
    responseLBS,
  )
import Network.Wai.Middleware.Cors
  ( cors,
    corsExposedHeaders,
    corsMethods,
    corsOrigins,
    corsRequestHeaders,
    simpleCorsResourcePolicy,
    simpleHeaders,
    simpleMethods,
  )
import Network.Wai.Middleware.Gzip
  ( GzipFiles (GzipCacheFolder),
    def,
    gzip,
    gzipFiles,
  )
import Network.Wai.Middleware.Routed (routedMiddleware)
import RIO
import qualified RIO.ByteString as BS
import qualified RIO.ByteString.Lazy as BSL
import qualified RIO.Text as T
import SweetSpot.AppM
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Dashboard (validateSessionId')
import SweetSpot.Database.Queries.Fulcrum (validateDomain)
import SweetSpot.Database.Queries.Util (withConnIO)
import SweetSpot.Env (Environment (..))
import qualified SweetSpot.Logger as L

-- WAI doesn't seem to want to know about routing.
-- This should probably move into a Servant handler somehow.
gzipStatic :: Middleware
gzipStatic = routedMiddleware ("static" `elem`) (gzip settings)
  where
    settings = def {gzipFiles = GzipCacheFolder "./dist/"}

send400 :: BSL.ByteString -> Application
send400 msg _req sendResponse =
  sendResponse $
    responseLBS
      status400
      [(hContentType, "text/plain")]
      msg

send302 :: BSL.ByteString -> Application
send302 msg req sendResponse =
  sendResponse $
    responseLBS
      status302
      [(hLocation, "/api/oauth/install" <> qs)]
      msg
  where
    qs = rawQueryString req

verifyHmac :: AppCtx -> Middleware
verifyHmac ctx app req sendResponse =
  case (== digestTxt) <$> mSupplied of
    Just True -> app req sendResponse
    _ -> do
      let appLogger = ctx ^. ctxLogger
      L.warn' appLogger "Got invalid hmac digest"
      send400 "Invalid HMAC digest" req sendResponse
  where
    secret = ctx ^. ctxConfig . configShopifyClientSecret
    params = queryString req
    mSupplied = decodeUtf8Lenient <$> (L.find ((== "hmac") . fst) params >>= snd)
    sansHMAC = filter ((/= "hmac") . fst) params
    joined = mapMaybe (\(key, val) -> fmap (\v -> key <> "=" <> v) val) sansHMAC
    checkable = BS.intercalate "&" joined
    digest = hmacGetDigest $ hmac (encodeUtf8 secret) checkable :: Digest SHA256
    digestTxt = T.pack $ show digest

verifyProxySignature :: AppCtx -> Middleware
verifyProxySignature ctx app req sendResponse =
  case (== digestTxt) <$> mSupplied of
    Just True -> app req sendResponse
    _ -> do
      let appLogger = ctx ^. ctxLogger
      L.warn' appLogger "Got invalid signature"
      send400 "Invalid signature" req sendResponse
  where
    secret = ctx ^. ctxConfig . configShopifyClientSecret
    params = queryString req
    mSupplied = decodeUtf8Lenient <$> (L.find ((== "signature") . fst) params >>= snd)
    sansHMAC = filter ((/= "signature") . fst) params
    joined = mapMaybe (\(key, val) -> fmap (\v -> key <> "=" <> v) val) sansHMAC
    checkable = mconcat $ L.sort joined
    digest = hmacGetDigest $ hmac (encodeUtf8 secret) checkable :: Digest SHA256
    digestTxt = T.pack $ show digest

validateShopDomain :: AppCtx -> Middleware
validateShopDomain ctx app req sendResponse = do
  let pool = ctx ^. ctxDbPool
      appLogger = ctx ^. ctxLogger
      params = queryString req
      mSuppliedDomain =
        ShopDomain . decodeUtf8Lenient <$> (snd =<< L.find ((== "shop") . fst) params)
  L.info' appLogger $ T.pack . show $ params
  case mSuppliedDomain of
    Just domain -> do
      mShopDomain <- withConnIO pool $ \conn -> validateDomain conn domain
      case mShopDomain of
        Just _ -> app req sendResponse
        Nothing -> do
          L.warn' appLogger $ "Got invalid shop query parameter: " <> showText domain
          send400 "Got invalid shop query parameter" req sendResponse
    Nothing -> do
      L.warn' appLogger "Missing shop query parameter"
      send400 "Missing shop query parameter" req sendResponse

validateSession :: AppCtx -> Middleware
validateSession ctx app req sendResponse = do
  let pool = ctx ^. ctxDbPool
      appLogger = ctx ^. ctxLogger
      params = queryString req
      mSuppliedId =
        SessionId . decodeUtf8Lenient <$> (snd =<< L.find ((== "session") . fst) params)
  L.info' appLogger $ T.pack . show $ params
  case mSuppliedId of
    Just id -> do
      mShopDomain <- withConnIO pool $ \conn -> validateSessionId' conn id
      case mShopDomain of
        Just _ -> app req sendResponse
        Nothing -> do
          L.warn' appLogger $ "Got invalid session query parameter: " <> showText id
          send400 "Got invalid session query parameter" req sendResponse
    Nothing -> do
      L.warn' appLogger "Missing session query parameter"
      send400 "Missing session query parameter" req sendResponse

enableCors :: Middleware
enableCors =
  cors $ \_ ->
    Just $
      simpleCorsResourcePolicy
        { corsOrigins =
            Just (["http://localhost:1234"], True),
          corsRequestHeaders = "Content-Type" : simpleHeaders,
          corsMethods = simpleMethods,
          corsExposedHeaders =
            Just ["Set-Cookie", "Access-Control-Allow-Origin", "Content-Type"]
        }

getMiddleware :: AppCtx -> Middleware
getMiddleware ctx =
  case env of
    -- So we don't have to deal with hmac during dev
    Dev -> gzipStatic . validateShopDomainRouted . enableCors . validateSessionRouted
    -- So we can focus on testing handlers themselves
    TestBusiness -> gzipStatic
    -- Else everything
    _ -> gzipStatic . verifySignatureRouted . verifyHmacRouted . validateShopDomainRouted . validateSessionRouted
  where
    env = ctx ^. ctxConfig . configEnvironment
    matchDashboardApp paths = elem "dashboard" paths && elem "index.html" paths
    -- Fulcrum endpoints are called through Shopify app proxy, and so
    -- need to be hmac validated. For some reason this works slightly differently
    -- from hmac query param for dashboard routes
    signatureVerifiedRoutes = elem "fulcrum"
    -- Authentication request for the dashboard contains an hmac.
    hmacVerifiedRoutes paths = elem "oauth" paths || matchDashboardApp paths
    -- Domain verification applies to all the hmac verified routes, except
    -- OAuth ones since shop doesn't exist yet during installation
    domainVerifiedRoutes paths =
      hmacVerifiedRoutes paths && notElem "oauth" paths
    -- All dashboard APIs rely on session to identify the shop
    sessionVerifiedRoutes paths =
      elem "api" paths
        && elem "dashboard" paths
    verifySignatureRouted = routedMiddleware signatureVerifiedRoutes (verifyProxySignature ctx)
    verifyHmacRouted =
      routedMiddleware hmacVerifiedRoutes (verifyHmac ctx)
    validateShopDomainRouted =
      routedMiddleware domainVerifiedRoutes (validateShopDomain ctx)
    validateSessionRouted =
      routedMiddleware sessionVerifiedRoutes (validateSession ctx)
