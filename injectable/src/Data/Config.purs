module SweetSpot.Data.Config where

import Prelude

apiUrl :: String
apiUrl = "/apps/sweetspot/api"

hiddenPriceId :: String
hiddenPriceId = "sweetspot__price--hidden"

productClass :: String
productClass = "sweetspot__product"

idClass :: String
idClass = "sweetspot__price_id--"

eventEndpoint :: String
eventEndpoint = apiUrl <> "/event"

logEndpoint :: String
logEndpoint = apiUrl <> "/log"

experimentEndpoint :: String
experimentEndpoint = apiUrl <> "/bucket"

variantUrlPattern :: String
variantUrlPattern = "-ssv"

campaignIdQueryParam :: String
campaignIdQueryParam = "sscid"

uidStorageKey :: String
uidStorageKey = "sweetspot__uid"

data DryRunMode
  = DryRun
  | Live

dryRunMode :: DryRunMode
dryRunMode = Live
