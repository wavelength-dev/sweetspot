module SweetSpot.Env where

import LoadEnv (loadEnv)
import System.Envy (FromEnv, (.!=), decodeEnv, envMaybe, fromEnv)

data EnvConfig = EnvConfig
  { dbHost :: String
  , dbName :: String
  , dbPassword :: String
  , dbPort :: Int
  , dbUser :: String
  , environment :: String
  , shopifyApiRoot :: String
  , shopifyClientId :: String
  , shopifyClientSecret :: String
  , shopifyOAuthAccessToken :: String
  } deriving (Show)


libertyPriceApiRoot = "https://libertyprice.myshopify.com/admin/api/2019-07"
libertyPriceClientId = "634b531a6568d6eb076c2ad5c7e0265a"
libertyPriceClientSecret = "***REMOVED***"
libertyPriceOAuthAccessToken = "***REMOVED***"

instance FromEnv EnvConfig where
  fromEnv =
    EnvConfig <$> envMaybe "DB_HOST" .!= "localhost" <*>
    envMaybe "DB_NAME" .!= "sweetspot" <*>
    envMaybe "DB_PASSWORD" .!= "" <*>
    envMaybe "DB_PORT" .!= 5432 <*>
    envMaybe "DB_USER" .!= "sweetspot" <*>
    envMaybe "ENVIRONMENT" .!= "dev" <*>
    envMaybe "SHOPIFY_API_ROOT" .!= libertyPriceApiRoot <*>
    envMaybe "SHOPIFY_CLIENT_ID" .!= libertyPriceClientId <*>
    envMaybe "SHOPIFY_CLIENT_SECRET" .!= libertyPriceClientSecret <*>
    envMaybe "SHOPIFY_OAUTH_ACCESS_TOKEN" .!= libertyPriceOAuthAccessToken

getEnvConfig :: IO (Either String EnvConfig)
getEnvConfig = do
  loadEnv
  decodeEnv :: IO (Either String EnvConfig)
