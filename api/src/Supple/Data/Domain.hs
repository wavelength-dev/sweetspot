{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Supple.Data.Domain where

import Control.Lens.TH (makeLenses)
import Data.Aeson (ToJSON)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Supple.Data.Common (BucketId, EventId, OrderId, Svid, UserId)

-- | ---------------------------------------------------------------------------
-- | CheckoutEvent
-- | ---------------------------------------------------------------------------
data CheckoutEvent = CheckoutEvent
  { _chkId :: !EventId
  , _chkUserId :: !UserId
  , _chkBucketId :: !BucketId
  , _chkOrderId :: !OrderId
  , _chkTimestamp :: !UTCTime
  , _chkLineItems :: ![Svid]
  } deriving (Eq, Generic, Show)

makeLenses ''CheckoutEvent

instance ToJSON CheckoutEvent
