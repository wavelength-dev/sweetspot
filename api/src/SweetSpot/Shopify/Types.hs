{-# LANGUAGE TemplateHaskell #-}

module SweetSpot.Shopify.Types where

import Control.Lens
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Scientific (FPFormat (..), formatScientific)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import RIO
import qualified RIO.Text as T
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
  parseJSON = withObject "LineItem" $ \v -> do
    svid <- v .: "variant_id"
    quantity <- v .: "quantity"
    let txtSvid = T.pack $ formatScientific Fixed (Just 0) svid
    return
      LineItem
        { _lineItemVariantId = Svid txtSvid,
          _lineItemQuantity = quantity
        }

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

-- | ---------------------------------------------------------------------------
-- | RedactShop
-- | ---------------------------------------------------------------------------
data RedactShop
  = RedactShop
      { _redactShopId :: Int,
        _redactShopDomain :: ShopDomain
      }
  deriving (Generic, Show)

instance FromJSON RedactShop where
  parseJSON = withObject "RedactShop" $ \v ->
    RedactShop
      <$> v .: "shop_id"
      <*> v .: "shop_domain"

-- | ---------------------------------------------------------------------------
-- | Customer
-- | ---------------------------------------------------------------------------
data Customer
  = Customer
      { _customerId :: Int,
        _customerEmail :: Text,
        _customerPhone :: Text
      }
  deriving (Show)

instance FromJSON Customer where
  parseJSON = withObject "Customer" $ \v ->
    Customer
      <$> v .: "id"
      <*> v .: "email"
      <*> v .: "phone"

-- | ---------------------------------------------------------------------------
-- | RedactCustomer
-- | ---------------------------------------------------------------------------
data RedactCustomer
  = RedactCustomer
      { _redactCustomerShopId :: Int,
        _redactCustomerShopDomain :: ShopDomain,
        _redactCustomerCustomer :: Customer,
        _redactCustomerOrders :: [Int]
      }
  deriving (Show)

instance FromJSON RedactCustomer where
  parseJSON = withObject "RedactCustomer" $ \v ->
    RedactCustomer
      <$> v .: "shop_id"
      <*> v .: "shop_domain"
      <*> v .: "customer"
      <*> v .: "orders_to_redact"

-- | ---------------------------------------------------------------------------
-- | RequestData
-- | ---------------------------------------------------------------------------
data RequestData
  = RequestData
      { _dataRequestShopId :: Int,
        _dataRequestShopDomain :: ShopDomain,
        _dataRequestCustomer :: Customer,
        _dataRequestOrders :: [Int]
      }
  deriving (Show)

instance FromJSON RequestData where
  parseJSON = withObject "RequestData" $ \v ->
    RequestData
      <$> v .: "shop_id"
      <*> v .: "shop_domain"
      <*> v .: "customer"
      <*> v .: "orders_requested"

-- | ---------------------------------------------------------------------------
-- | WebhookTopic
-- | ---------------------------------------------------------------------------
data WebhookTopic
  = OrdersCreate
  | ShopRedact
  | CustomersRedact
  | CustomersDataRequest

instance Show WebhookTopic where
  show = \case
    OrdersCreate -> "orders/create"
    ShopRedact -> "shop/redact"
    CustomersRedact -> "customers/redact"
    CustomersDataRequest -> "customers/data_request"

-- | ---------------------------------------------------------------------------
-- | ShopInfo
-- | ---------------------------------------------------------------------------
data ShopInfo
  = ShopInfo
      { _shopInfoEmail :: !Text,
        _shopInfoCountryCode :: !Text,
        _shopInfoCurrency :: !Text,
        _shopInfoMoneyFormat :: !MoneyFormat
      }

instance FromJSON ShopInfo where
  parseJSON = withObject "ShopInfo" $ \v ->
    ShopInfo
      <$> v .: "email"
      <*> v .: "country_code"
      <*> v .: "currency"
      <*> v .: "money_format"

makeLenses ''ShopInfo

-- | ---------------------------------------------------------------------------
-- | ShopInfoResponse
-- | ---------------------------------------------------------------------------
data ShopInfoResponse
  = ShopInfoResponse
      { _shopInfoResponseShop :: !ShopInfo
      }

instance FromJSON ShopInfoResponse where
  parseJSON = withObject "ShopInfoResponse" $ \v ->
    ShopInfoResponse <$> v .: "shop"
