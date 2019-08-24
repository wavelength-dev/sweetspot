module SweetSpot.Main where

import Prelude
import Data.Array.NonEmpty (head)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error, throw)
import SweetSpot.AppM (AppM, ShortCircuit(..), applyFacadeUrl, applyPriceVariations, attachPriceObserver, ensureDeps, getCampaignId, getSiteId, getUserBuckets, getUserId, getUserBucketProvisions, overrideCheckout, runAppM, setUserId, unhidePrice)
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
  siteId <- getSiteId
  ubp <- getUserBucketProvisions mUid mCid
  buckets <- getUserBuckets ubp
  setUserId (head buckets)
  overrideCheckout siteId buckets
  applyPriceVariations buckets
  attachPriceObserver buckets
  trackView

logResult :: forall a. Either Error a -> Effect Unit
logResult = case _ of
  Left err -> runAff_ (\_ -> Console.error $ show err) (postLogPayload $ show err)
  Right _ -> pure unit

main :: Effect Unit
main =
  runAff_ logResult do
    result <- runAppM app
    liftEffect $ unhidePrice
    liftEffect
      $ case result of
          Left (ReportErr { message }) -> throw message
          Left (Noop reason) -> runAff_ (\_ -> Console.error reason) (postLogPayload reason)
          Right _ -> pure unit
