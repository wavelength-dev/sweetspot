module SweetSpot.Env where

import Partial.Unsafe (unsafeCrashWith)

foreign import rawAppEnv :: String

data AppEnv = Local | Remote

appEnv :: AppEnv
appEnv = case rawAppEnv of
  "local" -> Local
  "remote" -> Remote
  _ -> unsafeCrashWith "Unrecognized App Environment"

type Config
  = { apiUrl :: String }

config :: Config
config = case appEnv of
  Local -> { apiUrl: "//localhost:8082" }
  Remote -> { apiUrl: "" }

apiUrl :: String
apiUrl = config.apiUrl
