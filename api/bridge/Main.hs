module Main where

import Data.Proxy (Proxy (..))
import Language.PureScript.Bridge
  ( buildBridge,
    equal,
    mkSumType,
    writePSTypesWith,
  )
import Language.PureScript.Bridge.CodeGenSwitches (useGenRep)
import RIO
import qualified SweetSpot.Data.Api as Api
import TypeBridges (sweetspotBridge)

writeDashboardTypes :: IO ()
writeDashboardTypes =
  writePSTypesWith useGenRep "../dashboard/src" (buildBridge sweetspotBridge) myTypes
  where
    myTypes =
      [ let p = (Proxy :: Proxy Api.InfResult)
         in equal p (mkSumType p),
        let p = (Proxy :: Proxy Api.UICampaign)
         in equal p (mkSumType p),
        let p = (Proxy :: Proxy Api.UITreatment)
         in equal p (mkSumType p),
        let p = (Proxy :: Proxy Api.UITreatmentVariant)
         in equal p (mkSumType p),
        let p = (Proxy :: Proxy Api.Image)
         in equal p (mkSumType p),
        let p = (Proxy :: Proxy Api.Variant)
         in equal p (mkSumType p),
        let p = (Proxy :: Proxy Api.Product)
         in equal p (mkSumType p),
        let p = (Proxy :: Proxy Api.CartTokenReq)
         in equal p (mkSumType p)
      ]

main :: IO ()
main = writeDashboardTypes
