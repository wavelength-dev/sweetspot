module SweetSpot.Shopify.Types where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)

class FromShopJSON a where
  parseShopJSON :: Value -> Parser a

class ToShopJSON a where
  toShopJSON :: a -> Value

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

-- | ---------------------------------------------------------------------------
-- | CreateWebhookReq
-- | ---------------------------------------------------------------------------
data CreateWebhookData = CreateWebhookData
  { topic :: Text
  , address :: Text
  , format :: Text
  } deriving (Generic)

instance ToJSON CreateWebhookData

newtype CreateWebhookReq = CreateWebhookReq
  { webhook :: CreateWebhookData
  } deriving (Generic)

instance ToJSON CreateWebhookReq
