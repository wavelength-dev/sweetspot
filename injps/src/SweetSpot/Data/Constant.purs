module SweetSpot.Data.Constant where

import Data.String as S
import Prelude ((<>))
import SweetSpot.Env (apiURL) as Env

hiddenPriceId ::
  String
hiddenPriceId = "sweetspot__price--hidden"

productClass ::
  String
productClass = "sweetspot__product"

uidStorageKey ::
  String
uidStorageKey = "sweetspot_uid_1554053013187"

idClassPattern ::
  S.Pattern
idClassPattern = S.Pattern "sweetspot__price_id--"

eventEndpoint ::
  String
eventEndpoint = Env.apiURL <> "/event"

logEndpoint ::
  String
logEndpoint = Env.apiURL <> "/log"

experimentEndpoint ::
  String
experimentEndpoint = Env.apiURL <> "/bucket"
