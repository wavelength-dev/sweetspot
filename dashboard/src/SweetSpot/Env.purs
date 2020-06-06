module SweetSpot.Env where

import Partial.Unsafe (unsafeCrashWith)

foreign import appEnv :: String

type Config
  = { apiUrl :: String }

config :: Config
config = case appEnv of
  "local" -> { apiUrl: "//localhost:8082" }
  "remote" -> { apiUrl: "/apps/sweetspot" }
  _ -> unsafeCrashWith "Unrecognized App Environment"

apiUrl :: String
apiUrl = config.apiUrl
