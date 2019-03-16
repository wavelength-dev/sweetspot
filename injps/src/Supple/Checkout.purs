module Supple.Checkout where

import Prelude

import Control.Monad.Except
  ( runExcept
  )
import Data.Either
  ( Either(..)
  )
import Data.List.NonEmpty
  ( NonEmptyList
  )
import Data.Maybe
  ( Maybe(..)
  )
import Debug.Trace
  ( trace
  )
import Effect
  ( Effect
  )
import Effect.Aff
  ( Aff
  , Error
  , attempt
  , launchAff_
  )
import Effect.Class
  ( liftEffect
  )
import Foreign
  ( Foreign
  , ForeignError
  , readNumber
  , readString
  )
import Foreign.Index
  ( readProp
  )
import Global.Unsafe
  ( unsafeStringify
  )
import Milkis
  ( Response
  )
import Milkis.Impl.Window
  ( windowFetch
  )
import Record
  ( merge
  )
import Supple.Data.Constant
  ( apiRoot
  )
import Supple.Request
  ( jsonHeader
  , postLogPayload
  )
import Web.HTML
  ( window
  )
import Web.HTML.Location
  ( href
  )
import Web.HTML.Window
  ( location
  )

import Milkis as M

fetch ::
  M.Fetch
fetch = M.fetch windowFetch

type CheckoutA
  = Foreign

type CheckoutB
  = Foreign

foreign import checkoutA ::
  CheckoutA

foreign import checkoutB ::
  CheckoutB

type CheckoutStateA
  = {step :: Maybe String, token :: Maybe String}

type CheckoutStateB
  = {lineItems :: Maybe Foreign, orderId :: Maybe Number}

type CheckoutState
  = {step :: Maybe String, token :: Maybe String, lineItems :: Maybe Foreign, orderId :: Maybe Number}

getCheckoutState ::
  CheckoutA ->
  CheckoutB ->
  CheckoutState
getCheckoutState chA chB = { step
                           , token
                           , lineItems
                           , orderId
                           }
  where
  step = safeExtract $ runExcept $ readProp "step" chA >>= readString
  token = safeExtract $ runExcept $ readProp "token" chA >>= readString
  lineItems = safeExtract $ runExcept $ readProp "line_items" chB
  orderId = safeExtract $ runExcept $ readProp "order_id" chB >>= readNumber
  safeExtract ::
    forall a.
    Either (NonEmptyList ForeignError) a ->
    Maybe a
  -- safeExtract :: Either (NonEmptyList ForeignError) String -> Maybe String
  safeExtract (Left errs) = Nothing
  safeExtract (Right val) = Just val

isCheckoutPage ::
  Unit ->
  Boolean
isCheckoutPage = \_ ->
  true

type CheckoutEvent
  = {lineItems :: Maybe Foreign, orderId :: Maybe Number, page :: String, pageUrl :: String, step :: Maybe String, token :: Maybe String}

postEvent ::
  CheckoutEvent ->
  Aff (Either Error Response)
postEvent event = attempt $ fetch (M.URL $ apiRoot <> "/event") opts
  where
  opts = { method: M.postMethod
         , headers: jsonHeader
         , body: strBody
         }
  strBody = (trace (unsafeStringify event) \_ ->
    unsafeStringify event)

getPageUrl ::
  Effect String
getPageUrl = window >>= location >>= href

trackCheckout ::
  Aff Unit
trackCheckout = do
  pageUrl <- liftEffect $ getPageUrl
  let r1 = getCheckoutState checkoutA checkoutB
  let r2 = { page: "checkout"
           , pageUrl
           }
  void $ postEvent $ merge r1 r2

main ::
  Effect Unit
main = launchAff_ $ if isCheckoutPage unit
  then (trace "heyo" \_ ->
    trackCheckout)
  else (trace "neyo" \_ ->
    postLogPayload "Failed to track checkout")
