module SweetSpot.Main where

import Prelude

import Data.Array (catMaybes)
import Data.Array.NonEmpty (head)
import Data.Either (Either(..))
import Data.Foldable (any, traverse_)
import Data.Maybe (isNothing)
import Effect (Effect)
import Effect.Aff (apathize, launchAff_, runAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, throw)
import SweetSpot.AppM (AppM, ShortCircuit(..), applyFacadeUrl, applyPriceVariations, attachPriceObserver, ensureDeps, getCampaignId, getUserBuckets, getUserId, maybeEarlyExit, runAppM, setUserId)
import SweetSpot.DOM (collectPriceEls, getDOMReady, removeClass)
import SweetSpot.Data.Constant (hiddenPriceId)
import SweetSpot.Event (trackView)
import SweetSpot.Request (postLogPayload)
import Web.HTML.HTMLElement (fromElement)

unhidePrice :: Effect Unit
unhidePrice = do
  priceElements <- collectPriceEls
  -- It's unlikely but possible not all collected Elements are HTMLElements
  -- We should only add sweetspot ids to elements which are HTMLElements
  -- In case we made a mistake, we log a warning and continue with those elements which are HTMLElements
  let
    priceHTMLElements = map fromElement priceElements

    anyInvalidElements = any isNothing priceHTMLElements
  _ <- when anyInvalidElements (launchAff_ $ postLogPayload "WARN: some collected price elements are not HTMLElements")
  traverse_ (removeClass hiddenPriceId) (catMaybes $ priceHTMLElements)

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
    Right _ -> launchAff_ $ apathize $ postLogPayload "Successfully ran SweetSpot"
