{-# LANGUAGE DeriveGeneric #-}

module Supple.Data.Common where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Generics (Generic)

class ToDatabaseInt a where
  toDatabaseInt :: a -> Int64

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
  Svid Int
  deriving (Eq, Show, Generic)

instance ToJSON Svid

instance FromJSON Svid

instance ToDatabaseInt Svid where
  toDatabaseInt (Svid i) = fromIntegral i

-- | ---------------------------------------------------------------------------
-- | Pid
-- | ---------------------------------------------------------------------------
newtype Pid =
  Pid Int
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

-- | ---------------------------------------------------------------------------
-- | UserId
-- | ---------------------------------------------------------------------------
newtype UserId =
  UserId Int
  deriving (Eq, Show, Generic)

instance ToJSON UserId

instance FromJSON UserId

instance ToDatabaseInt UserId where
  toDatabaseInt (UserId i) = fromIntegral i

-- | ---------------------------------------------------------------------------
-- | ExpId
-- | ---------------------------------------------------------------------------
newtype ExpId =
  ExpId Int
  deriving (Eq, Show, Generic)

instance ToJSON ExpId

instance FromJSON ExpId

instance ToDatabaseInt ExpId where
  toDatabaseInt (ExpId i) = fromIntegral i

-- | ---------------------------------------------------------------------------
-- | BucketId
-- | ---------------------------------------------------------------------------
newtype BucketId =
  BucketId Int
  deriving (Eq, Show, Generic)

instance ToJSON BucketId

instance FromJSON BucketId

instance ToDatabaseInt BucketId where
  toDatabaseInt (BucketId i) = fromIntegral i

-- | ---------------------------------------------------------------------------
-- | CampaignId
-- | ---------------------------------------------------------------------------
newtype CampaignId =
  CampaignId Text
  deriving (Eq, Show, Generic)

instance ToJSON CampaignId

instance FromJSON CampaignId

-- | ---------------------------------------------------------------------------
-- | EventType
-- | ---------------------------------------------------------------------------
data EventType =
  View
