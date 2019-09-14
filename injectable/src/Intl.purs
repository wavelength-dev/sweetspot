module SweetSpot.Intl where

import Effect (Effect)
import Prelude ((>>=))

foreign import data NumberFormat :: Type

foreign import _numberFormat :: forall a. String -> a -> Effect NumberFormat

numberFormat :: Effect NumberFormat
numberFormat = _numberFormat "en-US" { style: "currency", currency: "USD", minimumFractionDigits: 0, maximumFractionDigits: 0 }

foreign import formatNumber :: NumberFormat -> Number -> Effect String

formatPrice :: Number -> Effect String
formatPrice price = numberFormat >>= \nf -> formatNumber nf price
