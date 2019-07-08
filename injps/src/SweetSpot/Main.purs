module SweetSpot.Main where

import Prelude

import Data.Array.NonEmpty (head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (forkAff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import SweetSpot.AppM (AppM, ClientErr(..), runAppM)
import SweetSpot.Capability (applyPriceVariations, attachPriceObserver, ensureCampaign, ensureDeps, getUserBuckets, getUserId, log, setUserId)
import SweetSpot.DOM (getDOMReady, unhidePrice)
import SweetSpot.Event (trackView)
import SweetSpot.Request (postLogPayload)

app :: AppM Unit
app = do
  liftAff getDOMReady
  ensureDeps
  uid <- getUserId
  campaignId <- ensureCampaign uid
  buckets <- getUserBuckets uid campaignId
  setUserId (head buckets)
  mApplied <- applyPriceVariations buckets
  case mApplied of
    Nothing -> log "Unable to reveal prices for some collected price elements"
    Just _ -> pure unit
  attachPriceObserver buckets
  trackView (head buckets)

main :: Effect Unit
main = launchAff_ $ do
  result <- runAppM app
  liftEffect $ case result of
    Right _ -> pure unit
    Left (ClientErr { message }) -> do
      _ <- unhidePrice
      launchAff_ $ forkAff $ postLogPayload message
