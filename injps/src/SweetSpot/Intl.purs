module SweetSpot.Intl where

import Effect (Effect)

foreign import data NumberFormat :: Type

foreign import numberFormatImpl :: forall a. String -> a -> Effect NumberFormat

numberFormat :: Effect NumberFormat
numberFormat = numberFormatImpl "en-US" { style: "currency", currency: "USD" }

foreign import formatNumber :: Number -> NumberFormat -> Effect String
