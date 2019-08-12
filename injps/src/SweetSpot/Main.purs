module SweetSpot.Main where

import Prelude
import Data.Array.NonEmpty (head)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (apathize, launchAff_, runAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, throw)
import SweetSpot.AppM (AppM, ShortCircuit(..), applyFacadeUrl, applyPriceVariations, attachPriceObserver, ensureDeps, getCampaignId, getUserBuckets, getUserId, maybeEarlyExit, runAppM, setUserId, unhidePrice)
import SweetSpot.DOM (getDOMReady)
import SweetSpot.Event (trackView)
import SweetSpot.Request (postLogPayload)

app :: AppM Unit
app = do
  liftAff getDOMReady
  applyFacadeUrl
  ensureDeps
  uid <- getUserId
  cid <- getCampaignId
  maybeEarlyExit uid cid
  buckets <- getUserBuckets uid cid
  setUserId (head buckets)
  applyPriceVariations buckets
  attachPriceObserver buckets
  trackView
  liftEffect unhidePrice

main :: Effect Unit
main =
  runAff_ logResult
    $ do
        result <- runAppM app
        case result of
          Left (ReportErr { message }) -> liftEffect $ throw message
          Left Noop -> pure unit
          Right _ -> pure unit
  where
  logResult :: Either Error Unit -> Effect Unit
  logResult either = case either of
    -- If posting this log message fails there is little more we can do to report it so we ignore the result.
    Left err -> do
      -- If running our main logic encountered some problem we will still try to unhide the price as-is
      unhidePrice
      launchAff_ $ apathize $ postLogPayload (show err)
    Right _ -> pure unit
