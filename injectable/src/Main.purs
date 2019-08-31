module SweetSpot.Main where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_, runAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error, throw)
import SweetSpot.Api (postLogPayload)
import SweetSpot.AppM (AppM, ShortCircuit(..), applyFacadeUrl, applyPriceVariations, attachPriceObserver, ensureDeps, fixCartItemUrls, getCampaignId, getSiteId, getUserBucketProvisions, getUserBuckets, getUserId, runAppM, setCheckout, setUserId, unhidePrice)
import SweetSpot.Event (trackView)
import SweetSpot.SiteCapabilities (awaitDomReady)

app :: AppM Unit
app = do
  ensureDeps
  liftAff awaitDomReady
  liftEffect applyFacadeUrl
  mUid <- liftEffect $ getUserId
  mCid <- liftEffect $ getCampaignId
  siteId <- liftEffect $ getSiteId
  ubp <- getUserBucketProvisions mUid mCid
  testMaps <- getUserBuckets ubp
  liftEffect $ setUserId (NonEmptyArray.head testMaps)
  setCheckout siteId (NonEmptyArray.toArray testMaps)
  liftEffect $ applyPriceVariations testMaps
  liftEffect $ attachPriceObserver testMaps
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
