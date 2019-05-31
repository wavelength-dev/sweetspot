module Supple.Env where

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
  } deriving (Show)


libertyPriceApiRoot = "https://libertyprice.myshopify.com/admin"

instance FromEnv EnvConfig where
  fromEnv =
    EnvConfig <$> envMaybe "DB_HOST" .!= "localhost" <*>
    envMaybe "DB_NAME" .!= "supple" <*>
    envMaybe "DB_PASSWORD" .!= "" <*>
    envMaybe "DB_PORT" .!= 5432 <*>
    envMaybe "DB_USER" .!= "supple" <*>
    envMaybe "ENVIRONMENT" .!= "dev" <*>
    envMaybe "SHOPIFY_API_ROOT" .!= libertyPriceApiRoot

getEnvConfig :: IO (Either String EnvConfig)
getEnvConfig = do
  loadEnv
  decodeEnv :: IO (Either String EnvConfig)
