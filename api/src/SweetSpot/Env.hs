{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Env
  ( getEnvConfig
  , EnvConfig(..)
  , Environment(..)
  )
where

import           Data.Text                      ( Text )
import           LoadEnv                        ( loadEnv )
import           System.Envy                    ( FromEnv
                                                , decodeEnv
                                                , env
                                                , fromEnv
                                                , envMaybe
                                                , (.!=)
                                                , Var(..)
                                                )

data Environment = Dev | Stag | Prod | TestBusiness | TestHttp
  deriving (Eq, Show)

instance Var Environment where
  fromVar env = case env of
    "dev"  -> Just Dev
    "stag" -> Just Stag
    "prod" -> Just Prod
    "test_business" -> Just TestBusiness
    "test_http" -> Just TestHttp
    _      -> Nothing
  toVar = show

data EnvConfig = EnvConfig
  { dbHost :: !Text
  , dbName :: !Text
  , dbPassword :: !Text
  , environment :: !Environment
  , shopifyClientSecret :: !Text
  , shopifyOAuthAccessToken :: !Text
  , shopifyOAuthRedirectUri :: !Text
  , targetShop :: !Text
  , basicAuthUser :: !Text
  , basicAuthPassword :: !Text
  , port :: !Text
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
      <*> env "SHOPIFY_OAUTH_REDIRECT_URI"
      <*> env "TARGET_SHOP"
      <*> env "BASIC_AUTH_USER"
      <*> env "BASIC_AUTH_PASSWORD"
      <*> envMaybe "PORT" .!= "8082"

getEnvConfig :: IO (Either String EnvConfig)
getEnvConfig = do
  loadEnv
  decodeEnv
