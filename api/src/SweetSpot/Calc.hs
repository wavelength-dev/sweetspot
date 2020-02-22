module SweetSpot.Calc
  ( InfParams
  , runInference
  )
where

import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.List as L
import Statistics.Resampling
import Statistics.Sample (mean)
import System.Random.MWC (createSystemRandom, GenIO, uniformR)

data InfParams = InfParams
  { _conversions :: Vector Double
  , _nilCount :: Int
  } deriving (Show)

data InfResult = InfResult
  { _lowerBound :: Double
  , _upperBound :: Double
  , _mean :: Double
  } deriving (Show)

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
  lifts <- mapM (const $ compareRandomPair gen cMeans tMeans) [0..nBoot]
  let
    sorted = V.fromList $ L.sort lifts
    len = V.length sorted
    nTails = floor $ fromIntegral len * 0.05
    middle90 = V.slice nTails (len - nTails) sorted

  return InfResult
    { _mean = mean sorted
    , _lowerBound = V.head middle90
    , _upperBound = V.last middle90
    }

  where
    resample' gen s =
      resamples . snd . L.head <$> resample gen [Mean] 1000 s
