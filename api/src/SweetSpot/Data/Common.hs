{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module SweetSpot.Data.Common where

import Database.Beam.Backend.SQL
import Database.Beam.Query (HasSqlEqualityCheck(..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (Scientific)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)

-- | ---------------------------------------------------------------------------
-- | Price
-- | ---------------------------------------------------------------------------
newtype Price =
  Price Scientific
  deriving (Eq, Show, Generic)

instance ToJSON Price

instance FromJSON Price

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
  UserId Int
  deriving (Eq, Show, Generic, Ord)

instance ToJSON UserId

instance FromJSON UserId

-- | ---------------------------------------------------------------------------
-- | ExpId
-- | ---------------------------------------------------------------------------
newtype ExpId =
  ExpId Int
  deriving (Eq, Show, Generic)

instance ToJSON ExpId

instance FromJSON ExpId

-- | ---------------------------------------------------------------------------
-- | BucketId
-- | ---------------------------------------------------------------------------
newtype BucketId =
  BucketId Int
  deriving (Eq, Show, Generic)

instance ToJSON BucketId

instance FromJSON BucketId

-- | ---------------------------------------------------------------------------
-- | CampaignId
-- | ---------------------------------------------------------------------------
newtype CampaignId =
  CampaignId Text
  deriving (Eq, Show, Generic)

instance ToJSON CampaignId

instance FromJSON CampaignId

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be CampaignId where
  sqlValueSyntax = sqlValueSyntax . \(CampaignId txt) -> txt

instance (BeamSqlBackend be, FromBackendRow be Text) => FromBackendRow be CampaignId where
  fromBackendRow = CampaignId <$> fromBackendRow

instance (BeamSqlBackend be, HasSqlEqualityCheck be Text) => HasSqlEqualityCheck be CampaignId


-- | ---------------------------------------------------------------------------
-- | OrderId
-- | ---------------------------------------------------------------------------
newtype OrderId =
  OrderId Int
  deriving (Eq, Show, Generic)

instance ToJSON OrderId

-- | ---------------------------------------------------------------------------
-- | EventId
-- | ---------------------------------------------------------------------------
newtype EventId =
  EventId Int
  deriving (Eq, Show, Generic)

instance ToJSON EventId

-- | ---------------------------------------------------------------------------
-- | EventType
-- | ---------------------------------------------------------------------------
data EventType
  = View
  | Checkout
  | Log

eventTypeToText :: EventType -> Text
eventTypeToText Checkout = "checkout"
eventTypeToText Log = "log"
eventTypeToText View = "view"

-- | ---------------------------------------------------------------------------
-- | BucketType
-- | ---------------------------------------------------------------------------
data BucketType
  = Control
  | Test
  deriving (Eq, Generic, Show)

instance ToJSON BucketType

instance FromJSON BucketType

bucketTypeToText :: BucketType -> Text
bucketTypeToText Control = "control"
bucketTypeToText Test = "test"

bucketTypeFromText :: Text -> BucketType
bucketTypeFromText "control" = Control
bucketTypeFromText _ = Test
