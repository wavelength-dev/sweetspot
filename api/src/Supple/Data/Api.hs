{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Supple.Data.Api where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Supple.Data.Common (Price)

--
-- Request types
--
data TrackView = TrackView
  { page :: !Text
  } deriving (Generic, Show)

data CreateVariant = CreateVariant
  { option1 :: Text
  , price :: !Price
  } deriving (Generic, Show)

data CreateExperiment = CreateExperiment
  { productId :: !Int
  , price :: !Price
  , name :: !Text
  } deriving (Generic, Show)

instance FromJSON TrackView

instance ToJSON TrackView

instance ToJSON CreateVariant

instance FromJSON CreateVariant

instance ToJSON CreateExperiment

instance FromJSON CreateExperiment

--
-- Response types
--
data OkResponse = OkResponse
  { message :: Text
  } deriving (Generic, Show)

instance ToJSON OkResponse
