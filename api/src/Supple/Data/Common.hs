{-# LANGUAGE DeriveGeneric #-}

module Supple.Data.Common where

import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Price =
  Price Scientific
  deriving (Show, Generic)

newtype Svid =
  Svid Int
  deriving (Show, Generic)

newtype Sku =
  Sku Text
  deriving (Show, Generic)

newtype UserId =
  UserId Int
  deriving (Show, Generic)

newtype ExpId =
  ExpId Int
  deriving (Show, Generic)

newtype BucketId =
  BucketId Int
  deriving (Show, Generic)

instance ToJSON Price

instance FromJSON Price

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
