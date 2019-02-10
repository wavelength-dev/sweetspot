{-# LANGUAGE DeriveGeneric #-}

module Supple.Data.Common where

import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (Scientific)
import GHC.Generics (Generic)

newtype Price =
  Price Scientific
  deriving (Show, Generic)

instance ToJSON Price

instance FromJSON Price

data EventType =
  View
