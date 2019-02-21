module Main where

import Language.PureScript.Bridge (buildBridge, mkSumType, writePSTypes)
import Supple.Data.Api (CreateExperiment, OkResponse)

import Data.Proxy (Proxy(..))
import TypeBridges (suppleBridge)

main :: IO ()
main =
  writePSTypes "../frontend/dashboard/src" (buildBridge suppleBridge) myTypes
  where
    myTypes =
      [ mkSumType (Proxy :: Proxy OkResponse)
      , mkSumType (Proxy :: Proxy CreateExperiment)
      ]
