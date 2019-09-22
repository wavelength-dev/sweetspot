{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Env where

import           Data.Text                      ( Text )
import           LoadEnv                        ( loadEnv )
import           System.Envy                    ( FromEnv
                                                , decodeEnv
                                                , env
                                                , fromEnv
                                                , envMaybe
                                                , (.!=)
                                                )

data EnvConfig = EnvConfig
  { dbHost :: !Text
  , dbName :: !Text
  , dbPassword :: !Text
  , environment :: !Text
  , shopifyClientSecret :: !Text
  , shopifyOAuthAccessToken :: !Text
  , targetShop :: !Text
  } deriving (Show)

instance FromEnv EnvConfig where
  fromEnv _ =
    EnvConfig
      <$> env "DB_HOST"
      <*> env "DB_NAME"
      <*> envMaybe "DB_PASSWORD"
      .!= ""
      <*> env "ENVIRONMENT"
      <*> env "SHOPIFY_CLIENT_SECRET"
      <*> env "SHOPIFY_OAUTH_ACCESS_TOKEN"
      <*> env "TARGET_SHOP"

getEnvConfig :: IO (Either String EnvConfig)
getEnvConfig = do
  loadEnv
  decodeEnv :: IO (Either String EnvConfig)
