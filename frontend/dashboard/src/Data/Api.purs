module Data.Api where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, getField)
import Data.Either (Either)
import Data.Traversable (sequence)

type Bucket =
  { bucket_id :: Int
  , price :: Number}

type Experiment =
  { exp_id :: Int
  , sku :: String
  , buckets :: Array Bucket }

decodeBucket :: Json -> Either String Bucket
decodeBucket json = do
  obj <- decodeJson json
  bucket_id <- getField obj "bucket_id"
  price <- getField obj "price"
  pure { bucket_id, price }

decodeExperiment :: Json -> Either String Experiment
decodeExperiment json = do
  obj <- decodeJson json
  exp_id <- getField obj "exp_id"
  sku <- getField obj "sku"
  arr <- getField obj "buckets"
  buckets <- sequence $ decodeBucket <$> arr
  pure { exp_id, sku, buckets }


decodeResponse :: Json -> Either String (Array Experiment)
decodeResponse json = do
  arr <- decodeJson json
  sequence $ decodeExperiment <$> arr
