module SweetSpot.Env where

foreign import appEnv :: String

type Config
  = { apiUrl :: String }

config :: Config
config = case appEnv of
  "development" -> { apiUrl: "//localhost:8082" }
  _ -> { apiUrl: "/apps/sweetspot" }

apiUrl :: String
apiUrl = config.apiUrl
