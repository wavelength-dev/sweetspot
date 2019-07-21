module SweetSpot.Main where

import Prelude

import Data.Array (catMaybes, length)
import Data.Array.NonEmpty (head)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Aff (apathize, launchAff_, runAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, throw)
import SweetSpot.AppM (AppM, ClientErr(..), applyFacadeUrl, applyPriceVariations, attachPriceObserver, ensureCampaign, ensureDeps, getUserBuckets, getUserId, runAppM, setUserId)
import SweetSpot.DOM (collectPriceEls, getDOMReady, removeClass)
import SweetSpot.Data.Constant (hiddenPriceId)
import SweetSpot.Event (trackView)
import SweetSpot.Request (postLogPayload)
import Web.HTML.HTMLElement (fromElement)

unhidePrice :: Effect Unit
unhidePrice = do
  priceElements <- collectPriceEls
  let
    priceHTMLElements = catMaybes $ map fromElement priceElements
  -- It's unlikely but possible not all collected Elements are HTMLElements
  -- We should only add sweetspot ids to elements which are HTMLElements
  -- In case we made a mistake, we log a warning and continue with those elements which are HTMLElements
  _ <- if length priceElements /= length priceHTMLElements then
    launchAff_ $ postLogPayload "WARN: some collected price elements are not HTMLElements"
  else
    pure unit
  traverse_ (removeClass hiddenPriceId) priceHTMLElements

app :: AppM Unit
app = do
  liftAff getDOMReady
  applyFacadeUrl
  ensureDeps
  uid <- getUserId
  campaignId <- ensureCampaign uid
  buckets <- getUserBuckets uid campaignId
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
          Left (ClientErr { message }) -> liftEffect $ throw message
          Right _ -> pure unit
  where
  logResult :: Either Error Unit -> Effect Unit
  logResult either = case either of
    -- If posting this log message fails there is little more we can do to report it so we ignore the result.
    Left err -> do
      -- If running our main logic encountered some problem we will still try to unhide the price as-is
      _ <- unhidePrice
      launchAff_ $ apathize $ postLogPayload (show err)
    Right _ -> launchAff_ $ apathize $ postLogPayload "Successfully ran SweetSpot"
