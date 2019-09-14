module SweetSpot.Main where

import Prelude
import Control.Monad.Except (throwError)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_, runAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error, throw)
import SweetSpot.Api (postLogPayload)
import SweetSpot.AppM (AppM, ShortCircuit(..), Site(..), applyFacadeUrl, applyPriceVariations, ensureDeps, fixCartItemUrls, getCampaignId, getSiteId, getTestMaps, getUserBucketProvisions, getUserId, runAppM, setUserId, unhidePrice)
import SweetSpot.Data.Domain (getTestMapsByTargetId)
import SweetSpot.Event (trackView)
import SweetSpot.LibertyPrice (observePrices, setCheckout) as LP
import SweetSpot.Longvadon (attachObservers, setCheckout) as Lv
import SweetSpot.SiteCapabilities (awaitDomReady)

app :: AppM Unit
app = do
  ensureDeps
  liftAff awaitDomReady
  liftEffect applyFacadeUrl
  mUid <- liftEffect $ getUserId
  mCid <- liftEffect $ getCampaignId
  mSiteId <- liftEffect getSiteId
  site <- case mSiteId of
    Left hostname ->
      throwError
        $ ReportErr
            { message: "Site not recognized, can't control checkout, hostname was " <> hostname
            , payload: ""
            }
    Right site -> pure site
  ubp <- getUserBucketProvisions mUid mCid
  testMaps <- getTestMaps ubp
  let
    testMapsMap = getTestMapsByTargetId testMaps
  liftEffect $ setUserId (NonEmptyArray.head testMaps)
  liftEffect
    $ case site of
        LibertyPrice ->
          LP.setCheckout testMapsMap
          *> applyPriceVariations testMapsMap
          *> LP.observePrices testMapsMap
        Longvadon ->
          Lv.setCheckout testMapsMap
          *> applyPriceVariations testMapsMap
          *> Lv.attachObservers testMapsMap
  liftEffect $ fixCartItemUrls site
  liftEffect $ launchAff_ trackView

logResult :: forall a. Either Error a -> Effect Unit
logResult = case _ of
  Left appErr -> runAff_ (onLogPosted appErr) (postLogPayload $ show appErr)
  Right _ -> pure unit
  where
  -- If logging to the server failed we still log to console.
  onLogPosted appErr (Left logPostingErr) = Console.error $ format logPostingErr appErr

  onLogPosted _ (Right _) = pure unit

  format logErr appErr =
    "Failed to post log message, falling back to console.\n"
      <> "Logging error: "
      <> show logErr
      <> "App error: "
      <> show appErr

main :: Effect Unit
main =
  runAff_ logResult do
    result <- runAppM app
    liftEffect $ unhidePrice
    liftEffect
      $ case result of
          Left (ReportErr { message }) -> throw message
          Left (Noop reason) -> pure unit
          Right _ -> pure unit
