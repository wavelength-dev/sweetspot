{-# LANGUAGE TemplateHaskell #-}

module Supple.Data.Domain where

import Control.Lens.TH (makeLenses)
import Data.Time (UTCTime)
import Supple.Data.Common (BucketId, EventId, OrderId, UserId)

-- | ---------------------------------------------------------------------------
-- | CheckoutEvent
-- | ---------------------------------------------------------------------------
data CheckoutEvent = CheckoutEvent
  { _chkId :: !EventId
  , _chkUserId :: !UserId
  , _chkBucketId :: !BucketId
  , _chkOrderId :: !OrderId
  , _chkTimestamp :: !UTCTime
  }

makeLenses ''CheckoutEvent
