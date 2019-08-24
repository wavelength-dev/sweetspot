module SweetSpot.Env where

import LoadEnv (loadEnv)
import System.Envy (FromEnv, decodeEnv, env, fromEnv, envMaybe, (.!=))

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

instance FromEnv EnvConfig where
  fromEnv =
    EnvConfig <$> env "DB_HOST" <*>
    env "DB_NAME" <*>
    envMaybe "DB_PASSWORD" .!= "" <*>
    env "DB_PORT" <*>
    env "DB_USER" <*>
    env "ENVIRONMENT" <*>
    env "SHOPIFY_API_ROOT" <*>
    env "SHOPIFY_CLIENT_ID" <*>
    env "SHOPIFY_CLIENT_SECRET" <*>
    env "SHOPIFY_OAUTH_ACCESS_TOKEN"

getEnvConfig :: IO (Either String EnvConfig)
getEnvConfig = do
  loadEnv
  decodeEnv :: IO (Either String EnvConfig)
