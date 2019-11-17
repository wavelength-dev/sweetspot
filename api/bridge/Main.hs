module Main where

-- import Language.PureScript.Bridge
--   ( buildBridge
--   , equal
--   , mkSumType
--   , noLenses
--   , writePSTypes
--   , writePSTypesWith
--   )
-- import qualified SweetSpot.Data.Api as Api

-- import Data.Proxy (Proxy(..))
-- import TypeBridges (sweetspotBridge)

-- writeInjectableTypes :: IO ()
-- writeInjectableTypes =
--   writePSTypesWith noLenses "../injps/src" (buildBridge sweetspotBridge) myTypes
--   where
--     myTypes =
--       [ let p = (Proxy :: Proxy Api.UserBucket)
--          in equal p (mkSumType p)
--       ]

-- writeDashboardTypes :: IO ()
-- writeDashboardTypes =
--   writePSTypes "../dashboard/src" (buildBridge sweetspotBridge) myTypes
--   where
--     myTypes =
--       [ let p = (Proxy :: Proxy Api.Bucket)
--          in equal p (mkSumType p)
--       , let p = (Proxy :: Proxy Api.Experiment)
--          in equal p (mkSumType p)
--       , let p = (Proxy :: Proxy Api.ExperimentBuckets)
--          in equal p (mkSumType p)
--       , let p = (Proxy :: Proxy Api.Product)
--          in equal p (mkSumType p)
--       , let p = (Proxy :: Proxy Api.Variant)
--          in equal p (mkSumType p)
--       , let p = (Proxy :: Proxy Api.CreateExperiment)
--          in equal p (mkSumType p)
--       , let p = (Proxy :: Proxy Api.OkResponse)
--          in equal p (mkSumType p)
--       ]

main :: IO ()
main = print "lol"
