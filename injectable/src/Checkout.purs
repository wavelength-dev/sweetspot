module SweetSpot.Checkout where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Argonaut (encodeJson) as Argonaut
import Data.Argonaut (stringify)
import Data.Either (Either(..), hush)
import Data.List (foldr)
import Data.Maybe (Maybe(..))
import Data.Maybe (maybe) as Maybe
import Data.Number (fromString)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, Error, attempt, launchAff_)
import Effect.Class (liftEffect)
import Foreign (Foreign, F, readArray, readNumber, readString, renderForeignError)
import Foreign.Index (readProp, (!))
import Milkis (Response)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import Record (merge)
import SweetSpot.Data.Config (eventEndpoint, uidStorageKey)
import SweetSpot.Data.Event (CheckoutEvent, LineItem(..), Page(..))
import SweetSpot.Log (LogLevel(..))
import SweetSpot.Log (log) as Log
import Web.HTML (window)
import Web.HTML.Location (href)
import Web.HTML.Window (localStorage, location)
import Web.Storage.Storage (getItem)

fetch :: M.Fetch
fetch = M.fetch windowFetch

type CheckoutA = Foreign

type CheckoutB = Foreign

-- Shopify global with information on the current state of the checkout process.
-- Think step, payment provider, checkout id.
foreign import checkoutA :: CheckoutA

-- Shopify global with information about the initiated checkout.
-- Think basket, customer details, product id.
foreign import checkoutB :: CheckoutB

type CheckoutState =
  { lineItems :: Maybe (Array LineItem)
  , step :: Maybe String
  , token :: Maybe String
  , orderId :: Maybe Number
  }

readLineItem :: Foreign -> F LineItem
readLineItem value = do
  productId <- value ! "product_id" >>= readNumber
  variantId <- value ! "variant_id" >>= readNumber
  sku <- value ! "sku" >>= readString
  quantity <- value ! "quantity" >>= readNumber
  pure $ LineItem { productId, variantId, sku, quantity }

getCheckoutStateIssues :: CheckoutA -> CheckoutB -> Maybe String
getCheckoutStateIssues chA chB = case mErr of
  Left errors -> Just $ foldr (\e str ->
    str <> renderForeignError e) "" errors
  Right _ -> Nothing
  where
  mErr = runExcept do
    step <- readProp "step" chA >>= readString
    token <- readProp "token" chA >>= readString
    lineItems <- readProp "line_items" chB >>= readArray >>= traverse readLineItem
    orderId <- readProp "order_id" chB >>= readNumber
    pure unit

getCheckoutState :: CheckoutA -> CheckoutB -> CheckoutState
getCheckoutState chA chB = { step
                           , token
                           , lineItems
                           , orderId
                           }
  where
  step = hush $ runExcept $ readProp "step" chA >>= readString
  token = hush $ runExcept $ readProp "token" chA >>= readString
  lineItems = hush $ runExcept $ readProp "line_items" chB >>= readArray >>= traverse readLineItem
  orderId = hush $ runExcept $ readProp "order_id" chB >>= readNumber

trackEvent :: CheckoutEvent -> Aff (Either Error Response)
trackEvent event = attempt $ fetch (M.URL eventEndpoint) opts
  where
  opts = { method: M.postMethod
         , headers: M.makeHeaders { "Content-Type": "application/json" }
         , body: stringify $ Argonaut.encodeJson event
         }

getPageUrl :: Effect String
getPageUrl = window >>= location >>= href

trackCheckout :: Aff Unit
trackCheckout = do
  pageUrl <- liftEffect $ getPageUrl
  userId <- liftEffect $ window
    >>= localStorage
    >>= getItem uidStorageKey
    >>= (\mUid -> pure $ (mUid >>= fromString))
  let r1 = getCheckoutState checkoutA checkoutB
  let r2 = { page: Checkout
           , pageUrl
           , userId
           }
  void $ trackEvent $ merge r1 r2

main :: Effect Unit
main = launchAff_ $ do
  let err = getCheckoutStateIssues checkoutA checkoutB
  liftEffect $ Maybe.maybe (pure unit) (Log.log Error) err
  trackCheckout
