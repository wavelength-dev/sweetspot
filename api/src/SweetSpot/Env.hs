module SweetSpot.Env where

import           LoadEnv                        ( loadEnv )
import           System.Envy                    ( FromEnv
                                                , decodeEnv
                                                , env
                                                , fromEnv
                                                , envMaybe
                                                , (.!=)
                                                )

data EnvConfig = EnvConfig
  { dbHost :: String
  , dbName :: String
  , dbPassword :: String
  , dbPort :: Int
  , dbUser :: String
  , environment :: String
  , shopifyClientSecret :: String
  , shopifyOAuthAccessToken :: String
  , targetShop :: String
  } deriving (Show)

instance FromEnv EnvConfig where
  fromEnv _ =
    EnvConfig
      <$> env "DB_HOST"
      <*> env "DB_NAME"
      <*> envMaybe "DB_PASSWORD"
      .!= ""
      <*> env "DB_PORT"
      <*> env "DB_USER"
      <*> env "ENVIRONMENT"
      <*> env "SHOPIFY_CLIENT_SECRET"
      <*> env "SHOPIFY_OAUTH_ACCESS_TOKEN"
      <*> env "TARGET_SHOP"

getEnvConfig :: IO (Either String EnvConfig)
getEnvConfig = do
  loadEnv
  decodeEnv :: IO (Either String EnvConfig)
