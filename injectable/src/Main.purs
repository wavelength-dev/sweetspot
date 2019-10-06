module SweetSpot.Main where

import Prelude

import Control.Monad.Except (throwError)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, throw)
import SweetSpot.AppM (AppM, ShortCircuit(..), Site(..), applyFacadeUrl, ensureDeps, fixCartItemUrls, getSiteId, getTestMaps, getTestMapsBySku, getTestMapsByTargetId, getUserBucketProvisions, getUserId, readCampaignId, revealPrices, runAppM, setControlledPrices, setUserId)
import SweetSpot.LibertyPrice (observePrices, setCheckout) as LP
import SweetSpot.Log (LogLevel(..))
import SweetSpot.Log (log) as Log
import SweetSpot.Longvadon (attachObservers, setCheckout, setProductVariantSelectorSources, setProductAddToCartButtonControlledPrice) as Lv
import SweetSpot.SiteCapabilities (awaitDomReady) as SiteC

app :: AppM Unit
app = do
  ensureDeps
  liftAff SiteC.awaitDomReady
  liftEffect applyFacadeUrl
  mUid <- liftEffect $ getUserId
  mCid <- liftEffect $ readCampaignId
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
    testMapsMap' = getTestMapsBySku testMaps
  liftEffect $ setUserId (NonEmptyArray.head testMaps)
  liftEffect
    $ case site of
        LibertyPrice ->
          LP.setCheckout testMapsMap
            *> setControlledPrices testMapsMap
            *> LP.observePrices testMapsMap
        Longvadon ->
          Lv.setCheckout testMapsMap
            *> setControlledPrices testMapsMap
            *> Lv.setProductVariantSelectorSources testMapsMap'
            *> Lv.setProductAddToCartButtonControlledPrice testMapsMap
            *> Lv.attachObservers testMapsMap
  liftEffect $ fixCartItemUrls site

logResult :: forall a. Either Error a -> Effect Unit
logResult = case _ of
  Left appErr -> Log.log Error $ show appErr
  Right _ -> pure unit

main :: Effect Unit
main =
  runAff_ logResult do
    result <- runAppM app
    liftEffect $ revealPrices
    liftEffect
      $ case result of
          Left (ReportErr { message }) -> throw message
          -- On early exist we do nothing.
          Left Noop -> pure unit
          Right _ -> Log.log Info "Ran successfully for test"
