module SweetSpot.Data.Constant where

import Prelude ((<>))
import SweetSpot.Env as Env

hiddenPriceId :: String
hiddenPriceId = Env.hiddenPriceId

productClass :: String
productClass = Env.productClass

uidStorageKey :: String
uidStorageKey = Env.uidStorageKey

idClass :: String
idClass = Env.idClass

eventEndpoint :: String
eventEndpoint = Env.apiURL <> "/event"

logEndpoint :: String
logEndpoint = Env.apiURL <> "/log"

experimentEndpoint :: String
experimentEndpoint = Env.apiURL <> "/bucket"

variantUrlPattern :: String
variantUrlPattern = "-ssv"

campaignIdQueryParam :: String
campaignIdQueryParam = "sscid"

data DryRunMode
  = DryRun
  | Live

dryRunMode :: DryRunMode
dryRunMode = Live
