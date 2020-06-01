module SweetSpot.Middleware
  ( getMiddleware,
  )
where

import Crypto.Hash (Digest, SHA256)
import Crypto.MAC.HMAC
import Data.ByteArray.Encoding (Base (..), convertToBase)
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
    requestBody,
    requestHeaders,
    responseLBS,
    strictRequestBody,
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
  case (== digest) <$> mSupplied of
    Just True -> app req sendResponse
    Just False -> do
      L.warn' appLogger "Invalid hmac digest"
      send400 "Invalid HMAC digest" req sendResponse
    Nothing -> do
      L.warn' appLogger "Missing hmac digest"
      send400 "Missing HMAC digest" req sendResponse
  where
    appLogger = ctx ^. ctxLogger
    secret = ctx ^. ctxConfig . configShopifyClientSecret
    params = queryString req
    mSupplied =
      L.find ((== "hmac") . fst) params
        >>= snd
        & fmap decodeUtf8Lenient
    sansHMAC = filter ((/= "hmac") . fst) params
    joined = mapMaybe (\(key, val) -> fmap (\v -> key <> "=" <> v) val) sansHMAC
    checkable = BS.intercalate "&" joined
    digest = tshow (hmacGetDigest $ hmac (encodeUtf8 secret) checkable :: Digest SHA256)

verifyProxySignature :: AppCtx -> Middleware
verifyProxySignature ctx app req sendResponse =
  case (== digest) <$> mSupplied of
    Just True -> app req sendResponse
    Just False -> do
      L.warn' appLogger "Invalid signature"
      send400 "Invalid signature" req sendResponse
    Nothing -> do
      L.warn' appLogger "Missing signature"
      send400 "Missing signature" req sendResponse
  where
    appLogger = ctx ^. ctxLogger
    secret = ctx ^. ctxConfig . configShopifyClientSecret
    params = queryString req
    mSupplied =
      L.find ((== "signature") . fst) params
        >>= snd
        & fmap decodeUtf8Lenient
    sansHMAC = filter ((/= "signature") . fst) params
    joined = mapMaybe (\(key, val) -> fmap (\v -> key <> "=" <> v) val) sansHMAC
    checkable = mconcat $ L.sort joined
    digest = tshow (hmacGetDigest $ hmac (encodeUtf8 secret) checkable :: Digest SHA256)

verifyWebhookSignature :: AppCtx -> Middleware
verifyWebhookSignature ctx app req sendResponse = do
  body <- strictRequestBody req
  chunksRef <- newMVar $ BSL.toChunks body
  let chunkByChunk = modifyMVar chunksRef $ \chunks ->
        pure $ case chunks of
          [] -> mempty
          x : xs -> (xs, x)
      newReq = req {requestBody = chunkByChunk}
      digest = hmacGetDigest $ hmac (encodeUtf8 secret) (BSL.toStrict body) :: Digest SHA256
      encoded = convertToBase Base64 digest
  case (== encoded) <$> mSuppliedSignature of
    Just True -> app newReq sendResponse
    Just False -> do
      L.warn' appLogger "Got invalid webhook signature"
      send400 "Invalid webhook signature" newReq sendResponse
    Nothing -> do
      L.warn' appLogger "Missing webhook signature"
      send400 "Invalid webhook signature" newReq sendResponse
  where
    appLogger = ctx ^. ctxLogger
    secret = ctx ^. ctxConfig . configShopifyClientSecret
    mSuppliedSignature =
      requestHeaders req
        & L.find ((== "X-Shopify-Hmac-SHA256") . fst)
        & fmap snd

validateShopDomain :: AppCtx -> Middleware
validateShopDomain ctx app req sendResponse = do
  let pool = ctx ^. ctxDbPool
      appLogger = ctx ^. ctxLogger
      params = queryString req
      mSuppliedDomain =
        L.find ((== "shop") . fst) params
          >>= snd
          & fmap (ShopDomain . decodeUtf8Lenient)
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
        L.find ((== "session") . fst) params
          >>= snd
          & fmap (SessionId . decodeUtf8Lenient)
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
    Dev ->
      gzipStatic
        . validateShopDomainRouted
        . enableCors
        . validateSessionRouted
    -- So we can focus on testing handlers themselves
    TestBusiness -> gzipStatic
    -- Else everything
    _ ->
      gzipStatic
        . verifySignatureRouted
        . verifyHmacRouted
        . verifyWebhookRouted
        . validateShopDomainRouted
        . validateSessionRouted
  where
    env = ctx ^. ctxConfig . configEnvironment
    matchDashboardApp paths = elem "dashboard" paths && elem "index.html" paths
    -- Fulcrum endpoints are called through Shopify app proxy, and so
    -- need to be hmac validated. For some reason this works slightly differently
    -- from hmac query param for dashboard routes
    signatureVerifiedRoutes = elem "fulcrum"
    -- Authentication request for the dashboard contains an hmac.
    hmacVerifiedRoutes paths = elem "oauth" paths || matchDashboardApp paths
    webhookVerifiedRoutes = elem "webhook"
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
    verifyWebhookRouted =
      routedMiddleware webhookVerifiedRoutes (verifyWebhookSignature ctx)
    validateShopDomainRouted =
      routedMiddleware domainVerifiedRoutes (validateShopDomain ctx)
    validateSessionRouted =
      routedMiddleware sessionVerifiedRoutes (validateSession ctx)
