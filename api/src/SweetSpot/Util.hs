module SweetSpot.Util where

import RIO

nanToZero :: Double -> Double
nanToZero double = if isNaN double then 0 else double
