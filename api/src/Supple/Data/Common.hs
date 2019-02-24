{-# LANGUAGE DeriveGeneric #-}

module Supple.Data.Common where

import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Price =
  Price Scientific
  deriving (Eq, Show, Generic)

newtype Svid =
  Svid Int
  deriving (Eq, Show, Generic)

newtype Pid =
  Pid Int deriving (Eq, Show, Generic)

newtype Sku =
  Sku Text
  deriving (Eq, Show, Generic)

newtype UserId =
  UserId Int
  deriving (Eq, Show, Generic)

newtype ExpId =
  ExpId Int
  deriving (Eq, Show, Generic)

newtype BucketId =
  BucketId Int
  deriving (Eq, Show, Generic)

instance ToJSON Price

instance FromJSON Price

instance ToJSON Pid

instance FromJSON Pid

instance ToJSON Svid

instance FromJSON Svid

instance ToJSON Sku

instance FromJSON Sku

instance ToJSON UserId

instance FromJSON UserId

instance ToJSON ExpId

instance FromJSON ExpId

instance ToJSON BucketId

instance FromJSON BucketId

data EventType =
  View
