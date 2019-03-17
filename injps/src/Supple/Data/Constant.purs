module Supple.Data.Constant where

import Prelude
  ( (<>)
  )

import Data.String as S

hiddenPriceId ::
  String
hiddenPriceId = "supple__price--hidden"

productClass ::
  String
productClass = "supple__product"

uidStorageKey ::
  String
uidStorageKey = "supple_uid"

idClassPattern ::
  S.Pattern
idClassPattern = S.Pattern "supple__price_id--"

apiRoot ::
  String
apiRoot = "https://app.getsweetspot.com/api"

eventEndpoint ::
  String
eventEndpoint = apiRoot <> "/event"

logEndpoint ::
  String
logEndpoint = apiRoot <> "/log"

experimentEndpoint ::
  String
experimentEndpoint = apiRoot <> "/bucket"
