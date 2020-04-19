{-# LANGUAGE TemplateHaskell #-}

module SweetSpot.Shopify.Types where

import Control.Lens
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import RIO
import SweetSpot.Data.Common

class FromShopJSON a where
  parseShopJSON :: Value -> Parser a

class ToShopJSON a where
  toShopJSON :: a -> Value

-- | ---------------------------------------------------------------------------
-- | TokenExchangeReq
-- | ---------------------------------------------------------------------------
data TokenExchangeReq
  = TokenExchangeReq
      { client_id :: Text,
        client_secret :: Text,
        code :: Text
      }
  deriving (Generic)

instance ToJSON TokenExchangeReq

instance FromJSON TokenExchangeReq

-- | ---------------------------------------------------------------------------
-- | TokenExchangeRes
-- | ---------------------------------------------------------------------------
data TokenExchangeRes
  = TokenExchangeRes
      { access_token :: Text,
        scope :: Text
      }
  deriving (Generic)

instance ToJSON TokenExchangeRes

instance FromJSON TokenExchangeRes

-- | ---------------------------------------------------------------------------
-- | CreateWebhookReq
-- | ---------------------------------------------------------------------------
data CreateWebhookData
  = CreateWebhookData
      { topic :: Text,
        address :: Text,
        format :: Text
      }
  deriving (Generic)

instance ToJSON CreateWebhookData

newtype CreateWebhookReq
  = CreateWebhookReq
      { webhook :: CreateWebhookData
      }
  deriving (Generic)

instance ToJSON CreateWebhookReq

-- | ---------------------------------------------------------------------------
-- | LineItem
-- | ---------------------------------------------------------------------------
data LineItem
  = LineItem
      { _lineItemVariantId :: Svid,
        _lineItemQuantity :: Int
      }
  deriving (Show)

makeLenses ''LineItem

instance FromJSON LineItem where
  parseJSON = withObject "LineItem" $ \v ->
    LineItem
      <$> v .: "variant_id"
      <*> v .: "quantity"

-- | ---------------------------------------------------------------------------
-- | Order
-- | ---------------------------------------------------------------------------
data Order
  = Order
      { _orderId :: OrderId,
        _orderCartToken :: CartToken,
        _orderCreatedAt :: UTCTime,
        _orderLineItems :: [LineItem]
      }

makeLenses ''Order

instance FromJSON Order where
  parseJSON = withObject "Order" $ \v ->
    Order
      <$> v .: "id"
      <*> v .: "cart_token"
      <*> v .: "created_at"
      <*> v .: "line_items"
