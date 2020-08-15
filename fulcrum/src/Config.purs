module Fulcrum.Config where

apiUrl :: String
apiUrl = "/apps/sweetspot/api/fulcrum"

tokenStashKey :: String
tokenStashKey = "sweetspot_cart_tokens"

dryRunMap :: String -> Boolean
dryRunMap = case _ of
  "libertyprice.myshopify.com" -> true
  "establishedtitles.com" -> true
  _ -> false
