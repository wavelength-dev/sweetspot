{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}


module SweetSpot.Data.Common where

import           Database.Beam.Backend.SQL
import           Database.Beam.Query            ( HasSqlEqualityCheck(..) )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Scientific                ( Scientific )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.UUID.Types                ( UUID
                                                , toText
                                                , fromText
                                                )
import           GHC.Generics                   ( Generic )
import           Servant.API                    ( FromHttpApiData(..)
                                                , ToHttpApiData(..)
                                                )

class Show a => ShowText a where
  showText :: a -> Text

instance ShowText Int where
  showText = pack . show

-- | ---------------------------------------------------------------------------
-- | ShopId
-- | ---------------------------------------------------------------------------
newtype ShopId =
  ShopId UUID
  deriving (Eq, Show, Generic)

instance ToJSON ShopId

instance FromJSON ShopId

instance HasSqlValueSyntax be UUID => HasSqlValueSyntax be ShopId where
        sqlValueSyntax = sqlValueSyntax . \(ShopId uuid) -> uuid

instance (BeamSqlBackend be, FromBackendRow be UUID) => FromBackendRow be ShopId where
        fromBackendRow = ShopId <$> fromBackendRow

-- | ---------------------------------------------------------------------------
-- | ShopDomain
-- | ---------------------------------------------------------------------------
newtype ShopDomain =
  ShopDomain Text
  deriving (Eq, Show, Generic)

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
  showText = pack . show

-- | ---------------------------------------------------------------------------
-- | Price
-- | ---------------------------------------------------------------------------
newtype Price =
  Price Scientific
  deriving (Eq, Show, Generic)

instance ToJSON Price

instance FromJSON Price

instance HasSqlValueSyntax be Scientific => HasSqlValueSyntax be Price where
        sqlValueSyntax = sqlValueSyntax . \(Price p) -> p

instance (BeamSqlBackend be, FromBackendRow be Scientific) => FromBackendRow be Price where
        fromBackendRow = Price <$> fromBackendRow

instance (BeamSqlBackend be, HasSqlEqualityCheck be Scientific) => HasSqlEqualityCheck be Price

-- | ---------------------------------------------------------------------------
-- | Svid
-- | ---------------------------------------------------------------------------
newtype Svid =
  Svid Text
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
newtype Pid =
  Pid Text
  deriving (Eq, Show, Generic)

instance ToJSON Pid

instance FromJSON Pid

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Pid where
        sqlValueSyntax = sqlValueSyntax . \(Pid txt) -> txt

instance (BeamSqlBackend be, FromBackendRow be Text) => FromBackendRow be Pid where
        fromBackendRow = Pid <$> fromBackendRow

instance (BeamSqlBackend be, HasSqlEqualityCheck be Text) => HasSqlEqualityCheck be Pid

-- | ---------------------------------------------------------------------------
-- | Sku
-- | ---------------------------------------------------------------------------
newtype Sku =
  Sku Text
  deriving (Eq, Show, Generic)

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
newtype UserId =
  UserId UUID
  deriving (Eq, Show, Generic, Ord)

instance ToJSON UserId

instance FromJSON UserId

instance HasSqlValueSyntax be UUID => HasSqlValueSyntax be UserId where
        sqlValueSyntax = sqlValueSyntax . \(UserId uuid) -> uuid

instance (BeamSqlBackend be, FromBackendRow be UUID) => FromBackendRow be UserId where
        fromBackendRow = UserId <$> fromBackendRow

instance (BeamSqlBackend be, HasSqlEqualityCheck be UUID) => HasSqlEqualityCheck be UserId

instance FromHttpApiData UserId where
  parseQueryParam userId =
    case fromText userId of
      (Just uuid) -> Right $ UserId uuid
      Nothing -> Left "Got invalid UUID for userId"

instance ToHttpApiData UserId where
  toQueryParam (UserId uuid) = toText uuid

instance ShowText UserId where
  showText = pack . show

-- | ---------------------------------------------------------------------------
-- | PVariantId
-- | ---------------------------------------------------------------------------
newtype PVariantId =
  PVariantId UUID
  deriving (Eq, Show, Generic)

instance ToJSON PVariantId

instance FromJSON PVariantId

instance HasSqlValueSyntax be UUID => HasSqlValueSyntax be PVariantId where
        sqlValueSyntax = sqlValueSyntax . \(PVariantId id) -> id

instance (BeamSqlBackend be, FromBackendRow be UUID) => FromBackendRow be PVariantId where
        fromBackendRow = PVariantId <$> fromBackendRow

instance (BeamSqlBackend be, HasSqlEqualityCheck be Int) => HasSqlEqualityCheck be PVariantId

-- | ---------------------------------------------------------------------------
-- | BucketId
-- | ---------------------------------------------------------------------------
-- newtype BucketId =
--   BucketId UUID
--   deriving (Eq, Show, Generic)

-- instance ToJSON BucketId

-- instance FromJSON BucketId

-- instance HasSqlValueSyntax be UUID => HasSqlValueSyntax be BucketId where
--         sqlValueSyntax = sqlValueSyntax . \(BucketId uuid) -> uuid

-- instance (BeamSqlBackend be, FromBackendRow be UUID) => FromBackendRow be BucketId where
--         fromBackendRow = BucketId <$> fromBackendRow

-- instance (BeamSqlBackend be, HasSqlEqualityCheck be Int) => HasSqlEqualityCheck be BucketId

-- | ---------------------------------------------------------------------------
-- | CampaignId
-- | ---------------------------------------------------------------------------
newtype CampaignId =
  CampaignId UUID
  deriving (Eq, Show, Generic)

instance ToJSON CampaignId

instance FromJSON CampaignId

instance HasSqlValueSyntax be UUID => HasSqlValueSyntax be CampaignId where
        sqlValueSyntax = sqlValueSyntax . \(CampaignId id) -> id

instance (BeamSqlBackend be, FromBackendRow be UUID) => FromBackendRow be CampaignId where
        fromBackendRow = CampaignId <$> fromBackendRow

instance (BeamSqlBackend be, HasSqlEqualityCheck be UUID) => HasSqlEqualityCheck be CampaignId

instance FromHttpApiData CampaignId where
  parseQueryParam campaignId =
    case fromText campaignId of
      (Just uuid) -> Right $ CampaignId uuid
      Nothing -> Left "Got invalid UUID for campaignId"

instance ToHttpApiData CampaignId where
  toQueryParam (CampaignId uuid) = toText uuid

instance ShowText CampaignId where
  showText = pack . show

-- | ---------------------------------------------------------------------------
-- | OrderId
-- | ---------------------------------------------------------------------------
newtype OrderId =
  OrderId Text
  deriving (Eq, Show, Generic)

instance FromJSON OrderId

instance ToJSON OrderId

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be OrderId where
        sqlValueSyntax = sqlValueSyntax . \(OrderId id) -> id

instance (BeamSqlBackend be, FromBackendRow be Text) => FromBackendRow be OrderId where
        fromBackendRow = OrderId <$> fromBackendRow

instance (BeamSqlBackend be, HasSqlEqualityCheck be Text) => HasSqlEqualityCheck be OrderId

-- | ---------------------------------------------------------------------------
-- | EventId
-- | ---------------------------------------------------------------------------
newtype EventId =
  EventId UUID
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
                View     -> "view" :: Text
                Checkout -> "checkout" :: Text
                Log      -> "log" :: Text

instance (BeamSqlBackend be, FromBackendRow be Text) => FromBackendRow be EventType where
        fromBackendRow = do
                val <- fromBackendRow
                case val :: Text of
                        "view"     -> pure View
                        "checkout" -> pure Checkout
                        "log"      -> pure Log
                        _ ->
                                fail
                                        (  "Invalid value for EventType: "
                                        ++ unpack val
                                        )

instance (BeamSqlBackend be, HasSqlEqualityCheck be Text) => HasSqlEqualityCheck be EventType
