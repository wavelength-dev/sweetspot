module Main where

import Language.PureScript.Bridge (buildBridge, equal, mkSumType, writePSTypes)
import qualified Supple.Data.Api as Api

import Data.Proxy (Proxy(..))
import TypeBridges (suppleBridge)

main :: IO ()
main = writePSTypes "../dashboard/src" (buildBridge suppleBridge) myTypes
  where
    myTypes =
      [ let p = (Proxy :: Proxy Api.Bucket)
         in equal p (mkSumType p)
      , let p = (Proxy :: Proxy Api.Experiment)
         in equal p (mkSumType p)
      , let p = (Proxy :: Proxy Api.Product)
         in equal p (mkSumType p)
      , let p = (Proxy :: Proxy Api.Variant)
         in equal p (mkSumType p)
      , let p = (Proxy :: Proxy Api.CreateExperiment)
         in equal p (mkSumType p)
      , let p = (Proxy :: Proxy Api.OkResponse)
         in equal p (mkSumType p)
      ]
