module Supple.Data.Api where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, getField)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Traversable (sequence)

type Bucket =
  { bucket_id :: Int
  , price :: Number
  , svid :: Number}

type Experiment =
  { exp_id :: Int
  , sku :: String
  , name :: String
  , buckets :: Array Bucket }

type Experiments = Array Experiment

type ExperimentsResource = Maybe Experiments

type Variant =
  { id :: Int
  , sku :: String }

type Product =
  { id :: Int
  , title :: String
  , image :: String
  , variants :: Array Variant }

type Products = Array Product

type ProductsResource = Maybe Products

decodeBucket :: Json -> Either String Bucket
decodeBucket json = do
  obj <- decodeJson json
  bucket_id <- getField obj "bucket_id"
  price <- getField obj "price"
  svid <- getField obj "svid"
  pure { bucket_id, price, svid }

decodeExperiment :: Json -> Either String Experiment
decodeExperiment json = do
  obj <- decodeJson json
  exp_id <- getField obj "exp_id"
  sku <- getField obj "sku"
  name <- getField obj "name"
  arr <- getField obj "buckets"
  buckets <- sequence $ decodeBucket <$> arr
  pure { exp_id, sku, name, buckets }


decodeExperiments :: Json -> Either String Experiments
decodeExperiments json = do
  arr <- decodeJson json
  sequence $ decodeExperiment <$> arr

decodeVariant :: Json -> Either String Variant
decodeVariant json = do
  obj <- decodeJson json
  id <- getField obj "id"
  sku <- getField obj "sku"
  pure { id, sku }

decodeProduct :: Json -> Either String Product
decodeProduct json = do
  obj <- decodeJson json
  id <- getField obj "id"
  title <- getField obj "title"
  image <- (\i -> getField i "src") =<< getField obj "image"
  arr <- getField obj "variants"
  variants <- sequence $ decodeVariant <$> arr
  pure { id, title, image, variants }

decodeProducts :: Json -> Either String Products
decodeProducts json = sequence <<< (map decodeProduct) =<< decodeJson json
