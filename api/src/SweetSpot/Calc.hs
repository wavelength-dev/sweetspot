module SweetSpot.Calc
  ( InfParams (..),
    InfResult,
    runInference,
  )
where

import qualified Data.List as L
import RIO hiding (Vector)
import RIO.Vector.Unboxed (Vector)
import RIO.Vector.Unboxed as V hiding (mapM)
import RIO.Vector.Unboxed.Partial as VP
import Statistics.Resampling
import Statistics.Sample (mean)
import SweetSpot.Data.Api (InfResult (..))
import SweetSpot.Util (nanToZero)
import System.Random.MWC (GenIO, createSystemRandom, uniformR)

data InfParams
  = InfParams
      { _conversions :: Vector Double,
        _nilCount :: Int
      }
  deriving (Show)

nBoot = 1000

getSample :: InfParams -> Vector Double
getSample (InfParams cs nils) = cs <> V.replicate nils 0.0

randomElement :: GenIO -> Vector Double -> IO Double
randomElement gen v = do
  idx <- uniformR (0, (V.length v) - 1) gen
  return $ v ! idx

compareRandomPair :: GenIO -> Vector Double -> Vector Double -> IO Double
compareRandomPair gen cMeans tMeans = do
  cMean <- randomElement gen cMeans
  tMean <- randomElement gen tMeans
  return $ tMean / cMean

runInference :: InfParams -> InfParams -> IO InfResult
runInference cParams tParams = do
  gen <- createSystemRandom
  cMeans <- resample' gen (getSample cParams)
  tMeans <- resample' gen (getSample tParams)
  lifts <- mapM (const $ compareRandomPair gen cMeans tMeans) [0 .. nBoot]
  let sorted = V.fromList $ L.sort lifts
      len = V.length sorted
      nTails = floor $ fromIntegral len * 0.05
      middle90 = V.slice nTails (len - nTails) sorted
  return
    InfResult
      { _mean = nanToZero $ mean sorted,
        _lowerBound = nanToZero $ VP.head middle90,
        _upperBound = nanToZero $ VP.last middle90
      }
  where
    resample' gen s =
      resamples . snd . L.head <$> resample gen [Mean] 1000 s
