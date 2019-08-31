{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Database.Sessions where

import Control.Monad (forM)
import Data.Map.Strict as M
import Hasql.Session (Session)
import qualified Hasql.Session as Session
import SweetSpot.Data.Common
import SweetSpot.Database.Statements
import SweetSpot.Data.Domain
import SweetSpot.Data.Api
import Control.Lens ((^.))

-- | ---------------------------------------------------------------------------
-- | Dashboard sessions
-- | ---------------------------------------------------------------------------
getBucketsSession :: Session [ExperimentBuckets]
getBucketsSession = do
  experiments <- Session.statement () getExperimentsStatement
  forM experiments addBuckets
  where
    addBuckets experiment = do
      let experimentId = experiment ^. eExpId
      bs <- Session.statement experimentId getBucketsForExperimentStatement
      return
        ExperimentBuckets
          { _ebExpId = experimentId
          , _ebBuckets = bs
          , _ebProductName = experiment ^. eProductName
          , _ebSku = experiment ^. eSku
          }
