module Supple.Data.Codec where

import Prelude
import Supple.Data.Api (UserBucket(..))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either)
import Data.Traversable (sequence)

decodeUserBucket :: Json -> Either String UserBucket
decodeUserBucket json = do
  o <- decodeJson json
  _ubUserId <- o .: "_ubUserId"
  _ubSku  <- o .: "_ubSku"
  _ubSvid  <- o .: "_ubSvid"
  _ubPrice  <- o .: "_ubPrice"
  _ubExpId  <- o .: "_ubExpId"
  _ubBucketId  <- o .: "_ubBucketId"
  pure $ UserBucket
   { _ubUserId , _ubSku , _ubSvid , _ubPrice , _ubExpId , _ubBucketId}

decodeResponse :: String -> Either String (Array UserBucket)
decodeResponse s = do
  json <- jsonParser s
  arr <- decodeJson json
  sequence $ map decodeUserBucket arr
