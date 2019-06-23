module SweetSpot.Data.Constant where

import Data.String as S
import Prelude ((<>))
import SweetSpot.Env as Env

hiddenPriceId ::
  String
hiddenPriceId = Env.hiddenPriceId

productClass ::
  String
productClass = Env.productClass

uidStorageKey ::
  String
uidStorageKey = Env.uidStorageKey

idClassPattern ::
  S.Pattern
idClassPattern = S.Pattern Env.idClassPattern

eventEndpoint ::
  String
eventEndpoint = Env.apiURL <> "/event"

logEndpoint ::
  String
logEndpoint = Env.apiURL <> "/log"

experimentEndpoint ::
  String
experimentEndpoint = Env.apiURL <> "/bucket"
