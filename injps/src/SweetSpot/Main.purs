module SweetSpot.Main where

import Prelude
import Data.Array.NonEmpty (head)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, throw)
import SweetSpot.AppM (AppM, ShortCircuit(..), applyFacadeUrl, applyPriceVariations, attachPriceObserver, ensureDeps, getCampaignId, getUserBuckets, getUserId, getUserBucketProvisions, runAppM, setUserId, unhidePrice)
import SweetSpot.DOM (getDOMReady)
import SweetSpot.Event (trackView)
import SweetSpot.Request (postLogPayload)

app :: AppM Unit
app = do
  liftAff getDOMReady
  applyFacadeUrl
  ensureDeps
  mUid <- getUserId
  mCid <- getCampaignId
  ubp <- getUserBucketProvisions mUid mCid
  buckets <- getUserBuckets ubp
  setUserId (head buckets)
  applyPriceVariations buckets
  attachPriceObserver buckets
  trackView

logResult :: forall a. Either Error a -> Effect Unit
logResult either = case either of
  -- If posting this log message fails there is little more we can do to report it so we ignore the result.
  Left err ->
    runAff_
      (\_ -> pure unit)
      (postLogPayload (show err))
  Right _ -> pure unit

main :: Effect Unit
main =
  runAff_ logResult do
    result <- runAppM app
    liftEffect $ unhidePrice
    liftEffect
      $ case result of
          Left (ReportErr { message }) -> throw message
          Left Noop -> pure unit
          Right _ -> pure unit
