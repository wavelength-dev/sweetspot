{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module SweetSpot.Shopify.Client where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client hiding (Proxy)
import Servant
import Servant.Client

import SweetSpot.AppM
import SweetSpot.Data.Common
import SweetSpot.Env (Environment(..))
import SweetSpot.Shopify.Types


type TokenExchangeAPI =
  "admin" :> "oauth" :> "access_token"
  :> ReqBody '[JSON] TokenExchangeReq
  :> Post '[JSON] TokenExchangeRes

class Monad m => MonadShopify m where
  exchangeAccessToken :: ShopDomain -> Text -> m (Either Text Text)

testBaseUrl = BaseUrl { baseUrlScheme = Http
                      , baseUrlHost = "localhost"
                      , baseUrlPort = 9999
                      , baseUrlPath = ""
                      }

instance MonadShopify AppM where
  exchangeAccessToken domain code = do
    manager <- liftIO $ newManager defaultManagerSettings
    config <- asks _getConfig

    let
      baseUrl = case environment config of
        TestBusiness -> testBaseUrl
        _ -> BaseUrl { baseUrlScheme = Https
                     , baseUrlHost = show domain
                     , baseUrlPort = 443
                     , baseUrlPath = ""
                     }
      exchangeToken = client (Proxy :: Proxy TokenExchangeAPI)
      clientEnv = mkClientEnv manager baseUrl
      reqBody = TokenExchangeReq
        { client_id = shopifyClientId config
        , client_secret = shopifyClientSecret config
        , code = code
        }

    res <- liftIO $ runClientM (exchangeToken reqBody) clientEnv
    return $ case res of
      Left err -> Left $ "Error exchanging auth token: " <> (T.pack . show $ err)
      Right body -> Right $ access_token body
