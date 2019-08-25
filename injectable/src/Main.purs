module SweetSpot.Main where

import Prelude

import Data.Array.NonEmpty (head)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_, runAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error, throw)
import SweetSpot.AppM (AppM, ShortCircuit(..), applyFacadeUrl, applyPriceVariations, attachPriceObserver, ensureDeps, fixCartItemUrls, getCampaignId, getSiteId, getUserBucketProvisions, getUserBuckets, getUserId, overrideCheckout, runAppM, setUserId, unhidePrice)
import SweetSpot.DOM (awaitDomReady)
import SweetSpot.Event (trackView)
import SweetSpot.Request (postLogPayload)

app :: AppM Unit
app = do
  ensureDeps
  liftAff awaitDomReady
  liftEffect applyFacadeUrl
  mUid <- liftEffect $ getUserId
  mCid <- liftEffect $ getCampaignId
  siteId <- liftEffect $ getSiteId
  ubp <- getUserBucketProvisions mUid mCid
  buckets <- getUserBuckets ubp
  liftEffect $ setUserId (head buckets)
  overrideCheckout siteId buckets
  liftEffect $ applyPriceVariations buckets
  liftEffect $ attachPriceObserver buckets
  liftEffect $ fixCartItemUrls siteId

  liftEffect $ launchAff_ trackView

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
