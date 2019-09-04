module SweetSpot.Main where

import Prelude

import Control.Monad.Except (throwError)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Map (fromFoldable) as Map
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_, runAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error, throw)
import SweetSpot.Api (postLogPayload)
import SweetSpot.AppM (AppM, ShortCircuit(..), Site(..), applyFacadeUrl, applyPriceVariations, ensureDeps, fixCartItemUrls, getCampaignId, getSiteId, getTestMaps, getUserBucketProvisions, getUserId, runAppM, setUserId, unhidePrice)
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
    Left hostname -> throwError $ ReportErr { message: "Site not recognized, can't control checkout, hostname was " <> hostname, payload: "" }
    Right site -> pure site
  ubp <- getUserBucketProvisions mUid mCid
  testMaps <- getTestMaps ubp
  let
    testMapsMap = Map.fromFoldable $ map (\testMap -> Tuple testMap.targetId testMap) testMaps
  liftEffect $ setUserId (NonEmptyArray.head testMaps)
  liftEffect
    $ case site of
        LibertyPrice -> LP.setCheckout testMapsMap *> applyPriceVariations testMapsMap *> LP.observePrices testMapsMap
        Longvadon -> Lv.setCheckout testMapsMap *> applyPriceVariations testMapsMap *> Lv.attachObservers testMapsMap
  liftEffect $ fixCartItemUrls site
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
