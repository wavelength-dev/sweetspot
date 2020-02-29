module SweetSpot.Util where

nanToZero :: Double -> Double
nanToZero double = if isNaN double then 0 else double
