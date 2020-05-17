{-# LANGUAGE UndecidableInstances #-}

module SweetSpot.Data.Common where

import Data.Aeson
  ( FromJSON (..),
    ToJSON,
    withScientific,
  )
import Data.Scientific
  ( FPFormat (..),
    Scientific,
    formatScientific,
  )
import Data.UUID.Types
  ( UUID,
    fromText,
    toText,
  )
import Database.Beam.Backend.SQL
import Database.Beam.Query (HasSqlEqualityCheck (..))
import GHC.Generics (Generic)
import RIO
import qualified RIO.Text as T
import Servant.API
  ( FromHttpApiData (..),
    ToHttpApiData (..),
  )

class Show a => ShowText a where
  showText :: a -> Text

instance ShowText Int where
  showText = T.pack . show

-- | ---------------------------------------------------------------------------
-- | ShopId
-- | ---------------------------------------------------------------------------
newtype ShopId
  = ShopId UUID
  deriving (Eq, Show, Generic)

instance ToJSON ShopId

instance FromJSON ShopId

instance HasSqlValueSyntax be UUID => HasSqlValueSyntax be ShopId where
  sqlValueSyntax = sqlValueSyntax . \(ShopId uuid) -> uuid

instance (BeamSqlBackend be, FromBackendRow be UUID) => FromBackendRow be ShopId where
  fromBackendRow = ShopId <$> fromBackendRow

instance (BeamSqlBackend be, HasSqlEqualityCheck be UUID) => HasSqlEqualityCheck be ShopId

-- | ---------------------------------------------------------------------------
-- | ShopDomain
-- | ---------------------------------------------------------------------------
newtype ShopDomain
  = ShopDomain Text
  deriving (Eq, Generic)

instance Show ShopDomain where
  show (ShopDomain txt) = T.unpack txt

instance ToJSON ShopDomain

instance FromJSON ShopDomain

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be ShopDomain where
  sqlValueSyntax = sqlValueSyntax . \(ShopDomain text) -> text

instance (BeamSqlBackend be, FromBackendRow be Text) => FromBackendRow be ShopDomain where
  fromBackendRow = ShopDomain <$> fromBackendRow

instance (BeamSqlBackend be, HasSqlEqualityCheck be Text) => HasSqlEqualityCheck be ShopDomain

instance FromHttpApiData ShopDomain where
  parseQueryParam = Right . ShopDomain

instance ToHttpApiData ShopDomain where
  toQueryParam (ShopDomain txt) = txt

instance ShowText ShopDomain where
  showText (ShopDomain txt) = txt

-- | ---------------------------------------------------------------------------
-- | Price
-- | ---------------------------------------------------------------------------
newtype Price
  = Price Scientific
  deriving (Eq, Show, Generic, Num)

instance ToJSON Price

instance FromJSON Price

instance HasSqlValueSyntax be Scientific => HasSqlValueSyntax be Price where
  sqlValueSyntax = sqlValueSyntax . \(Price p) -> p

instance (BeamSqlBackend be, FromBackendRow be Scientific) => FromBackendRow be Price where
  fromBackendRow = Price <$> fromBackendRow

instance (BeamSqlBackend be, HasSqlEqualityCheck be Scientific) => HasSqlEqualityCheck be Price

instance ShowText Price where
  showText (Price n) = T.pack $ show n

-- | ---------------------------------------------------------------------------
-- | FormattedPrice
-- | ---------------------------------------------------------------------------
newtype FormattedPrice
  = FormattedPrice Text
  deriving (Generic, Show, Eq)

instance ToJSON FormattedPrice

instance FromJSON FormattedPrice

-- | ---------------------------------------------------------------------------
-- | Svid
-- | ---------------------------------------------------------------------------
newtype Svid
  = Svid Text
  deriving (Eq, Show, Generic)

instance ToJSON Svid

instance FromJSON Svid

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Svid where
  sqlValueSyntax = sqlValueSyntax . \(Svid txt) -> txt

instance (BeamSqlBackend be, FromBackendRow be Text) => FromBackendRow be Svid where
  fromBackendRow = Svid <$> fromBackendRow

instance (BeamSqlBackend be, HasSqlEqualityCheck be Text) => HasSqlEqualityCheck be Svid

-- | ---------------------------------------------------------------------------
-- | Pid
-- | ---------------------------------------------------------------------------
newtype Pid
  = Pid Text
  deriving (Eq, Show, Generic)

instance ToJSON Pid

instance FromJSON Pid

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Pid where
  sqlValueSyntax = sqlValueSyntax . \(Pid txt) -> txt

instance (BeamSqlBackend be, FromBackendRow be Text) => FromBackendRow be Pid where
  fromBackendRow = Pid <$> fromBackendRow

instance (BeamSqlBackend be, HasSqlEqualityCheck be Text) => HasSqlEqualityCheck be Pid

instance ToHttpApiData Pid where
  toQueryParam (Pid pid) = pid

-- | ---------------------------------------------------------------------------
-- | Sku
-- | ---------------------------------------------------------------------------
newtype Sku
  = Sku Text
  deriving (Eq, Show, Generic, Ord)

instance ToJSON Sku

instance FromJSON Sku

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Sku where
  sqlValueSyntax = sqlValueSyntax . \(Sku txt) -> txt

instance (BeamSqlBackend be, FromBackendRow be Text) => FromBackendRow be Sku where
  fromBackendRow = Sku <$> fromBackendRow

instance (BeamSqlBackend be, HasSqlEqualityCheck be Text) => HasSqlEqualityCheck be Sku

-- | ---------------------------------------------------------------------------
-- | UserId
-- | ---------------------------------------------------------------------------
newtype UserId
  = UserId Text
  deriving (Eq, Show, Generic, Ord)

instance ToJSON UserId

instance FromJSON UserId

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be UserId where
  sqlValueSyntax = sqlValueSyntax . \(UserId uuid) -> uuid

instance (BeamSqlBackend be, FromBackendRow be Text) => FromBackendRow be UserId where
  fromBackendRow = UserId <$> fromBackendRow

instance (BeamSqlBackend be, HasSqlEqualityCheck be Text) => HasSqlEqualityCheck be UserId

instance FromHttpApiData UserId where
  parseQueryParam = Right . UserId

instance ToHttpApiData UserId where
  toQueryParam (UserId uuid) = uuid

-- | ---------------------------------------------------------------------------
-- | PVariantId
-- | ---------------------------------------------------------------------------
newtype PVariantId
  = PVariantId UUID
  deriving (Eq, Show, Generic)

instance ToJSON PVariantId

instance FromJSON PVariantId

instance HasSqlValueSyntax be UUID => HasSqlValueSyntax be PVariantId where
  sqlValueSyntax = sqlValueSyntax . \(PVariantId id) -> id

instance (BeamSqlBackend be, FromBackendRow be UUID) => FromBackendRow be PVariantId where
  fromBackendRow = PVariantId <$> fromBackendRow

instance (BeamSqlBackend be, HasSqlEqualityCheck be Int) => HasSqlEqualityCheck be PVariantId

-- | ---------------------------------------------------------------------------
-- | CampaignId
-- | ---------------------------------------------------------------------------
newtype CampaignId
  = CampaignId UUID
  deriving (Eq, Show, Generic)

instance ToJSON CampaignId

instance FromJSON CampaignId

instance HasSqlValueSyntax be UUID => HasSqlValueSyntax be CampaignId where
  sqlValueSyntax = sqlValueSyntax . \(CampaignId id) -> id

instance (BeamSqlBackend be, FromBackendRow be UUID) => FromBackendRow be CampaignId where
  fromBackendRow = CampaignId <$> fromBackendRow

instance (BeamSqlBackend be, HasSqlEqualityCheck be UUID) => HasSqlEqualityCheck be CampaignId

instance FromHttpApiData CampaignId where
  parseQueryParam campaignId = case fromText campaignId of
    (Just uuid) -> Right $ CampaignId uuid
    Nothing -> Left "Got invalid UUID for campaignId"

instance ToHttpApiData CampaignId where
  toQueryParam (CampaignId uuid) = toText uuid

instance ShowText CampaignId where
  showText = T.pack . show

-- | ---------------------------------------------------------------------------
-- | OrderId
-- | ---------------------------------------------------------------------------
newtype OrderId
  = OrderId Text
  deriving (Eq, Show, Generic)

instance FromJSON OrderId where
  parseJSON =
    withScientific "OrderId" $ \v -> do
      let txt = T.pack $ formatScientific Fixed (Just 0) v
      return $ OrderId txt

instance ToJSON OrderId

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be OrderId where
  sqlValueSyntax = sqlValueSyntax . \(OrderId id) -> id

instance (BeamSqlBackend be, FromBackendRow be Text) => FromBackendRow be OrderId where
  fromBackendRow = OrderId <$> fromBackendRow

instance (BeamSqlBackend be, HasSqlEqualityCheck be Text) => HasSqlEqualityCheck be OrderId

-- | ---------------------------------------------------------------------------
-- | EventId
-- | ---------------------------------------------------------------------------
newtype EventId
  = EventId UUID
  deriving (Eq, Show, Generic)

instance ToJSON EventId

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be EventId where
  sqlValueSyntax = sqlValueSyntax . \(EventId uuid) -> toText uuid

instance (BeamSqlBackend be, FromBackendRow be UUID) => FromBackendRow be EventId where
  fromBackendRow = EventId <$> fromBackendRow

instance (BeamSqlBackend be, HasSqlEqualityCheck be Int) => HasSqlEqualityCheck be EventId

-- | ---------------------------------------------------------------------------
-- | EventType
-- | ---------------------------------------------------------------------------
data EventType
  = View
  | Checkout
  | Log

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be EventType where
  sqlValueSyntax = sqlValueSyntax . \case
    View -> "view" :: Text
    Checkout -> "checkout" :: Text
    Log -> "log" :: Text

instance (BeamSqlBackend be, FromBackendRow be Text) => FromBackendRow be EventType where
  fromBackendRow = do
    val <- fromBackendRow
    case val :: Text of
      "view" -> pure View
      "checkout" -> pure Checkout
      "log" -> pure Log
      _ ->
        fail
          ( "Invalid value for EventType: "
              ++ T.unpack val
          )

instance (BeamSqlBackend be, HasSqlEqualityCheck be Text) => HasSqlEqualityCheck be EventType

-- | ---------------------------------------------------------------------------
-- | Nonce
-- | ---------------------------------------------------------------------------
newtype Nonce
  = Nonce UUID
  deriving (Eq, Show)

instance FromHttpApiData Nonce where
  parseQueryParam qp = case fromText qp of
    Just uuid -> Right $ Nonce uuid
    Nothing -> Left "invalid nonce"

instance ToHttpApiData Nonce where
  toQueryParam (Nonce uuid) = toText uuid

instance HasSqlValueSyntax be UUID => HasSqlValueSyntax be Nonce where
  sqlValueSyntax = sqlValueSyntax . \(Nonce uuid) -> uuid

instance (BeamSqlBackend be, FromBackendRow be UUID) => FromBackendRow be Nonce where
  fromBackendRow = Nonce <$> fromBackendRow

instance ShowText Nonce where
  showText (Nonce uuid) = toText uuid

-- | ---------------------------------------------------------------------------
-- | OAuth stuff
-- | ---------------------------------------------------------------------------
newtype Code = Code Text

instance FromHttpApiData Code where
  parseQueryParam = Right . Code

instance ToHttpApiData Code where
  toQueryParam (Code code) = code

newtype HMAC' = HMAC' Text

instance FromHttpApiData HMAC' where
  parseQueryParam = Right . HMAC'

instance ToHttpApiData HMAC' where
  toQueryParam (HMAC' hmac) = hmac

newtype Timestamp = Timestamp Text

instance FromHttpApiData Timestamp where
  parseQueryParam = Right . Timestamp

instance ToHttpApiData Timestamp where
  toQueryParam (Timestamp ts) = ts

-- | ---------------------------------------------------------------------------
-- | SessionId
-- | ---------------------------------------------------------------------------
newtype SessionId
  = SessionId Text
  deriving (Eq, Show)

instance FromHttpApiData SessionId where
  parseQueryParam = Right . SessionId

instance ToHttpApiData SessionId where
  toQueryParam (SessionId sid) = sid

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be SessionId where
  sqlValueSyntax = sqlValueSyntax . \(SessionId s) -> s

instance (BeamSqlBackend be, HasSqlEqualityCheck be Text) => HasSqlEqualityCheck be SessionId

instance ShowText SessionId where
  showText (SessionId session) = session

-- | ---------------------------------------------------------------------------
-- | CartToken
-- | ---------------------------------------------------------------------------
newtype CartToken
  = CartToken Text
  deriving (Generic, Eq, Show)

instance FromJSON CartToken

instance ToJSON CartToken

instance FromHttpApiData CartToken where
  parseQueryParam = Right . CartToken

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be CartToken where
  sqlValueSyntax = sqlValueSyntax . \(CartToken t) -> t

instance (BeamSqlBackend be, HasSqlEqualityCheck be Text) => HasSqlEqualityCheck be CartToken

instance (BeamSqlBackend be, FromBackendRow be Text) => FromBackendRow be CartToken where
  fromBackendRow = CartToken <$> fromBackendRow

-- | ---------------------------------------------------------------------------
-- | MoneyFormat
-- | ---------------------------------------------------------------------------
newtype MoneyFormat
  = MoneyFormat Text
  deriving (Generic, Show)

instance FromJSON MoneyFormat

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be MoneyFormat where
  sqlValueSyntax = sqlValueSyntax . \(MoneyFormat mf) -> mf

instance (BeamSqlBackend be, FromBackendRow be Text) => FromBackendRow be MoneyFormat where
  fromBackendRow = MoneyFormat <$> fromBackendRow
