module Supple.Data.Codec where

import Debug.Trace
import Prelude
import Supple.Data.Api
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Encode ((:=), (~>))
import Data.Either (Either)
import Data.Lens ((^.))
import Data.Traversable (sequence)


decodeBucket :: Json -> Either String Bucket
decodeBucket json = do
  o <- decodeJson json
  _bBucketId <- o .: "_bBucketId"
  _bPrice <- o .: "_bPrice"
  _bSvid <- o .: "_bSvid"
  pure $ Bucket { _bBucketId, _bPrice, _bSvid }

decodeExperiment:: Json -> Either String Experiment
decodeExperiment json = do
  o <- decodeJson json
  _eExpId <- o .: "_ebExpId"
  _eSku <- o .: "_ebSku"
  _eName <- o .: "_ebName"
  pure $ Experiment { _eExpId, _eSku, _eName }

decodeVariant :: Json -> Either String Variant
decodeVariant json = do
  o <- decodeJson json
  _vId <- o .: "_vId"
  _vProductId <- o .: "_vProductId"
  _vTitle <- o .: "_vTitle"
  _vSku <- o .: "_vSku"
  pure $ Variant { _vId, _vProductId, _vTitle, _vSku }

decodeProduct :: Json -> Either String Product
decodeProduct json = do
  o <- decodeJson json
  _pId <- o .: "_pId"
  _pTitle <- o .: "_pTitle"
  _pVariants <- sequence <<< map decodeVariant =<< o .: "_pVariants"
  img <- o .: "_pImage"
  _pImage <- img .: "_iSrc"
  pure $ Product { _pId, _pTitle, _pVariants, _pImage }

decodeProductsRes :: Json -> Either String (Array Product)
decodeProductsRes json = sequence <<< map decodeProduct =<< decodeJson json

decodeExperimentsRes :: Json -> Either String (Array Experiment)
decodeExperimentsRes json = sequence <<< map decodeExperiment =<< decodeJson json

encodeCreateExperiment :: CreateExperiment -> Json
encodeCreateExperiment ce =
  "_ceProductId" := ce ^. ceProductId
  ~> "_cePrice" := ce ^. cePrice
  ~> "_ceName" := ce ^. ceName
  ~> jsonEmptyObject
