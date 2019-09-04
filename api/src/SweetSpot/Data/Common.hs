{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


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
import           GHC.Generics                   ( Generic )

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
  UserId Text
  deriving (Eq, Show, Generic, Ord)

instance ToJSON UserId

instance FromJSON UserId

instance HasSqlValueSyntax be Int => HasSqlValueSyntax be UserId where
        sqlValueSyntax = sqlValueSyntax . \(UserId id) -> read $ unpack id :: Int

usrIdFromRow :: Int -> UserId
usrIdFromRow = UserId . pack . show

instance (BeamSqlBackend be, FromBackendRow be Int) => FromBackendRow be UserId where
        fromBackendRow = usrIdFromRow <$> fromBackendRow

instance (BeamSqlBackend be, HasSqlEqualityCheck be Text) => HasSqlEqualityCheck be UserId

-- | ---------------------------------------------------------------------------
-- | ExpId
-- | ---------------------------------------------------------------------------
newtype ExpId =
  ExpId Int
  deriving (Eq, Show, Generic)

instance ToJSON ExpId

instance FromJSON ExpId

instance HasSqlValueSyntax be Int => HasSqlValueSyntax be ExpId where
        sqlValueSyntax = sqlValueSyntax . \(ExpId id) -> id

instance (BeamSqlBackend be, FromBackendRow be Int) => FromBackendRow be ExpId where
        fromBackendRow = ExpId <$> fromBackendRow

instance (BeamSqlBackend be, HasSqlEqualityCheck be Int) => HasSqlEqualityCheck be ExpId

-- | ---------------------------------------------------------------------------
-- | BucketId
-- | ---------------------------------------------------------------------------
newtype BucketId =
  BucketId Int
  deriving (Eq, Show, Generic)

instance ToJSON BucketId

instance FromJSON BucketId

instance HasSqlValueSyntax be Int => HasSqlValueSyntax be BucketId where
        sqlValueSyntax = sqlValueSyntax . \(BucketId id) -> id

instance (BeamSqlBackend be, FromBackendRow be Int) => FromBackendRow be BucketId where
        fromBackendRow = BucketId <$> fromBackendRow

instance (BeamSqlBackend be, HasSqlEqualityCheck be Int) => HasSqlEqualityCheck be BucketId

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

instance HasSqlValueSyntax be Int => HasSqlValueSyntax be EventId where
        sqlValueSyntax = sqlValueSyntax . \(EventId id) -> id

instance (BeamSqlBackend be, FromBackendRow be Int) => FromBackendRow be EventId where
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
        sqlValueSyntax = sqlValueSyntax . \evType -> case evType of
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

-- | ---------------------------------------------------------------------------
-- | BucketType
-- | ---------------------------------------------------------------------------
data BucketType
  = Control
  | Test
  deriving (Eq, Generic, Show)

instance ToJSON BucketType

instance FromJSON BucketType

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be BucketType where
        sqlValueSyntax = sqlValueSyntax . \evType -> case evType of
                Control -> "control" :: Text
                Test    -> "test" :: Text

instance (BeamSqlBackend be, FromBackendRow be Text) => FromBackendRow be BucketType where
        fromBackendRow = do
                val <- fromBackendRow
                case val :: Text of
                        "control" -> pure Control
                        "test"    -> pure Test
                        _ ->
                                fail
                                        (  "Invalid value for EventType: "
                                        ++ unpack val
                                        )

instance (BeamSqlBackend be, HasSqlEqualityCheck be Text) => HasSqlEqualityCheck be BucketType
