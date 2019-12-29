{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Mock.Shopify (withShopify) where


import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent as C
import Data.Text (Text)
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Servant

import SweetSpot.Route.Util
import SweetSpot.Shopify.Client (TokenExchangeAPI)
import SweetSpot.Shopify.Types

type AuthorizeAPI =
  "admin" :> "oauth" :> "authorize"
  :> QueryParam "client_id" Text
  :> QueryParam "scope" Text
  :> QueryParam "redirect_uri" Text
  :> QueryParam "state" Text
  :> Get303 '[PlainText] NoContent

type ShopifyAPI = AuthorizeAPI :<|> TokenExchangeAPI

authorizationHandler
  :: Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Handler (Headers '[Header "Location" Text] NoContent)
authorizationHandler _ _ _ (Just state) = do
  let
    redirectUri = "http://localhost:8082/api/oauth/redirect?code=lolcode&hmac=lolhmac&timestamp=1234&state=" <> state <> "&shop=localhost:9999"
  return $ addHeader redirectUri NoContent
authorizationHandler _ _ _ _ = throwError badRequestErr

getAccessTokenHandler :: TokenExchangeReq -> Handler TokenExchangeRes
getAccessTokenHandler _ =
  return TokenExchangeRes { access_token = "some-access-token", scope = "write_products,read_orders" }

shopifyServer :: Server ShopifyAPI
shopifyServer = authorizationHandler :<|> getAccessTokenHandler

shopifyApp :: Application
shopifyApp = serve (Proxy :: Proxy ShopifyAPI) shopifyServer

withShopify :: IO ()
withShopify = do
  liftIO $ C.forkIO $ Warp.run 9999 shopifyApp
  return ()
