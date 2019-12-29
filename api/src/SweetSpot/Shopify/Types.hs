{-# LANGUAGE DeriveGeneric #-}

module SweetSpot.Shopify.Types where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

-- | ---------------------------------------------------------------------------
-- | TokenExchangeReq
-- | ---------------------------------------------------------------------------
data TokenExchangeReq = TokenExchangeReq
  { client_id :: Text
  , client_secret :: Text
  , code :: Text
  } deriving (Generic)

instance ToJSON TokenExchangeReq
instance FromJSON TokenExchangeReq

-- | ---------------------------------------------------------------------------
-- | TokenExchangeRes
-- | ---------------------------------------------------------------------------
data TokenExchangeRes = TokenExchangeRes
  { access_token :: Text
  , scope :: Text
  } deriving (Generic)

instance ToJSON TokenExchangeRes
instance FromJSON TokenExchangeRes
