{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data UserBucket = UserBucket
  { bucket_sku :: !Text
  , bucket_svid :: !Int
  , bucket_price :: !Int
  } deriving (Generic, Show)

instance ToJSON UserBucket
