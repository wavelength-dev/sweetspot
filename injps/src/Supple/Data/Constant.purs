module Supple.Data.Constant where

import Data.String as S

hiddenPriceId :: String
hiddenPriceId = "supple__price--hidden"

uidStorageKey :: String
uidStorageKey = "supple_uid"

idClassPattern :: S.Pattern
idClassPattern = S.Pattern "supple__price_id--"

apiRoot :: String
apiRoot = "https://7d685827.ngrok.io/api"
