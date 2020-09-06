{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module SweetSpot.Shopify.Types where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Scientific
  ( FPFormat (..),
    Scientific (..),
    formatScientific,
  )
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import RIO
import RIO.Partial (read)
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

instance ToJSON LineItem where
  toJSON li =
    object
      [ "variant_id" .= view lineItemVariantId li,
        "quantity" .= view lineItemQuantity li
      ]

-- | ---------------------------------------------------------------------------
-- | Order
-- | ---------------------------------------------------------------------------
data Order
  = Order
      { _orderId :: OrderId,
        _orderCartToken :: Maybe CartToken,
        _orderCreatedAt :: UTCTime,
        _orderLineItems :: [LineItem]
      }
  deriving (Show)

makeLenses ''Order

instance FromJSON Order where
  parseJSON = withObject "Order" $ \v ->
    Order
      <$> v .: "id"
      <*> v .: "cart_token"
      <*> v .: "created_at"
      <*> v .: "line_items"

instance ToJSON Order where
  toJSON o =
    object
      [ "id" .= view orderId o,
        "cart_token" .= view orderCartToken o,
        "created_at" .= view orderCreatedAt o,
        "line_items" .= view orderLineItems o
      ]

-- | ---------------------------------------------------------------------------
-- | RedactShop
-- | ---------------------------------------------------------------------------
data RedactShop
  = RedactShop
      { _redactShopId :: Int,
        _redactShopDomain :: ShopDomain
      }
  deriving (Generic, Show)

makeLenses ''RedactShop

instance FromJSON RedactShop where
  parseJSON = withObject "RedactShop" $ \v ->
    RedactShop
      <$> v .: "shop_id"
      <*> v .: "shop_domain"

instance ToJSON RedactShop

-- | ---------------------------------------------------------------------------
-- | Customer
-- | ---------------------------------------------------------------------------
data Customer
  = Customer
      { _customerId :: Int,
        _customerEmail :: Text,
        _customerPhone :: Text
      }
  deriving (Show, Generic)

instance FromJSON Customer where
  parseJSON = withObject "Customer" $ \v ->
    Customer
      <$> v .: "id"
      <*> v .: "email"
      <*> v .: "phone"

instance ToJSON Customer

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
  deriving (Show, Generic)

makeLenses ''RedactCustomer

instance FromJSON RedactCustomer where
  parseJSON = withObject "RedactCustomer" $ \v ->
    RedactCustomer
      <$> v .: "shop_id"
      <*> v .: "shop_domain"
      <*> v .: "customer"
      <*> v .: "orders_to_redact"

instance ToJSON RedactCustomer

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
  deriving (Show, Generic)

makeLenses ''RequestData

instance FromJSON RequestData where
  parseJSON = withObject "RequestData" $ \v ->
    RequestData
      <$> v .: "shop_id"
      <*> v .: "shop_domain"
      <*> v .: "customer"
      <*> v .: "orders_requested"

instance ToJSON RequestData

-- | ---------------------------------------------------------------------------
-- | WebhookTopic
-- | ---------------------------------------------------------------------------
data WebhookTopic
  = AppUninstalled

instance Show WebhookTopic where
  show = \case
    AppUninstalled -> "app/uninstalled"

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

-- | ---------------------------------------------------------------------------
-- | ShopImage
-- | ---------------------------------------------------------------------------
data ShopImage
  = ShopImage
      { _shopImageSrc :: !Text
      }
  deriving (Eq, Generic, Show)

makeLenses ''ShopImage

instance ToJSON ShopImage

instance FromJSON ShopImage where
  parseJSON = withObject "ShopImage" $ \v ->
    ShopImage
      <$> v .: "src"

-- | ---------------------------------------------------------------------------
-- | ShopVariant
-- | ---------------------------------------------------------------------------
data ShopVariant
  = ShopVariant
      { _shopVariantId :: !Svid,
        _shopVariantProductId :: !Pid,
        _shopVariantTitle :: !Text,
        _shopVariantSku :: !Sku,
        _shopVariantPrice :: !Price
      }
  deriving (Eq, Generic, Show)

makeLenses ''ShopVariant

instance ToJSON ShopVariant

instance FromJSON ShopVariant where
  parseJSON = withObject "ShopVariant" $ \v -> do
    id <- v .: "id"
    productId <- v .: "product_id"
    title <- v .: "title"
    sku <- v .: "sku"
    price <- v .: "price"
    return
      ShopVariant
        { _shopVariantId = id & showText @Int & Svid,
          _shopVariantProductId = productId & showText @Int & Pid,
          _shopVariantTitle = title,
          _shopVariantSku = sku,
          _shopVariantPrice = price & read @Scientific & Price
        }

-- | ---------------------------------------------------------------------------
-- | ShopProduct
-- | ---------------------------------------------------------------------------
data ShopProduct
  = ShopProduct
      { _shopProductId :: !Pid,
        _shopProductTitle :: !Text,
        _shopProductVariants :: ![ShopVariant],
        _shopProductImage :: !ShopImage,
        _shopProductType :: !Text
      }
  deriving (Eq, Generic, Show)

makeLenses ''ShopProduct

instance ToJSON ShopProduct

instance FromJSON ShopProduct where
  parseJSON = withObject "ShopProduct" $ \v -> do
    id <- v .: "id"
    title <- v .: "title"
    variants <- v .: "variants" >>= traverse parseJSON
    image <- (v .: "image" >>= parseJSON) <|> pure ShopImage {_shopImageSrc = "notfound.jpg"}
    productType <- v .: "product_type"
    return
      ShopProduct
        { _shopProductId = id & showText @Int & Pid,
          _shopProductTitle = title,
          _shopProductVariants = variants,
          _shopProductImage = image,
          _shopProductType = productType
        }

-- | ---------------------------------------------------------------------------
-- | AppUninstalledReq
-- | ---------------------------------------------------------------------------
newtype AppUninstalledReq
  = AppUninstalledReq
      {_appUninstalledReqShopDomain :: ShopDomain}

instance FromJSON AppUninstalledReq where
  parseJSON = withObject "AppUninstalledReq" $ \v ->
    AppUninstalledReq <$> v .: "domain"

-- | ---------------------------------------------------------------------------
-- | CreateAppCharge
-- | ---------------------------------------------------------------------------
data CreateAppCharge
  = CreateAppCharge
      { _createAppChargeName :: !Text,
        _createAppChargePrice :: !Price,
        _createAppChargeReturnUrl :: !Text,
        _createAppChargeIsTest :: !Bool
      }

makeLenses ''CreateAppCharge

instance ToJSON CreateAppCharge where
  toJSON charge =
    object
      [ "recurring_application_charge"
          .= object
            [ "name" .= (charge ^. createAppChargeName),
              "price" .= (charge ^. createAppChargePrice),
              "return_url" .= (charge ^. createAppChargeReturnUrl),
              "test" .= (charge ^. createAppChargeIsTest)
            ]
      ]

-- | ---------------------------------------------------------------------------
-- | CreateAppChargeRes
-- | ---------------------------------------------------------------------------
data CreateAppChargeRes
  = CreateAppChargeRes
      { _createAppChargeResId :: !Text,
        _createAppChargeResName :: !Text,
        _createAppChargeResPrice :: !Text,
        _createAppChargeResStatus :: !AppChargeStatus,
        _createAppChargeResReturnUrl :: !Text,
        _createAppChargeResConfirmationUrl :: !Text
      }

makeLenses ''CreateAppChargeRes

instance FromJSON CreateAppChargeRes where
  parseJSON = withObject "CreateAppChargeRes" $ \v -> do
    o <- v .: "recurring_application_charge"
    id <- o .: "id"
    name <- o .: "name"
    price <- o .: "price"
    status <- o .: "status"
    returnUrl <- o .: "return_url"
    confirmationUrl <- o .: "confirmation_url"
    return
      CreateAppChargeRes
        { _createAppChargeResId = tshow @Int id,
          _createAppChargeResName = name,
          _createAppChargeResPrice = price,
          _createAppChargeResStatus = status,
          _createAppChargeResReturnUrl = returnUrl,
          _createAppChargeResConfirmationUrl = confirmationUrl
        }

-- | ---------------------------------------------------------------------------
-- | CreateScript
-- | ---------------------------------------------------------------------------
data CreateScript
  = CreateScript
      { _createScriptEvent :: Text,
        _createScriptSrc :: Text
      }

makeLenses ''CreateScript

instance ToJSON CreateScript where
  toJSON cs =
    object
      [ "script_tag"
          .= object
            [ "event" .= (cs ^. createScriptEvent),
              "src" .= (cs ^. createScriptSrc)
            ]
      ]

-- | ---------------------------------------------------------------------------
-- | SmartCollectionRule
-- | ---------------------------------------------------------------------------
data SmartCollectionRule
  = SmartCollectionRule
      { _smartCollectionRuleColumn :: !Text,
        _smartCollectionRuleRelation :: !Text,
        _smartCollectionRuleCondition :: !Text
      }

makeLenses ''SmartCollectionRule

instance ToJSON SmartCollectionRule where
  toJSON rule =
    object
      [ "column" .= (rule ^. smartCollectionRuleColumn),
        "relation" .= (rule ^. smartCollectionRuleRelation),
        "condition" .= (rule ^. smartCollectionRuleCondition)
      ]

instance FromJSON SmartCollectionRule where
  parseJSON = withObject "SmartCollectionRule" $ \o ->
    SmartCollectionRule
      <$> o .: "column"
      <*> o .: "relation"
      <*> o .: "condition"

-- | ---------------------------------------------------------------------------
-- | SmartCollectionRuleUpdate
-- | ---------------------------------------------------------------------------
newtype SmartCollectionRuleUpdate
  = SmartCollectionRuleUpdate [SmartCollectionRule]

makeLenses ''SmartCollectionRuleUpdate

instance ToJSON SmartCollectionRuleUpdate where
  toJSON (SmartCollectionRuleUpdate rules) =
    object
      [ "smart_collection"
          .= object
            [ "rules" .= (toJSON rules)
            ]
      ]
