module Fulcrum.Config where

apiUrl :: String
apiUrl = "/apps/sweetspot/api/fulcrum"

dryRunMap :: String -> Boolean
dryRunMap = case _ of
  "libertyprice.myshopify.com" -> true
  _ -> false
