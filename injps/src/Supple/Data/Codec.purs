module Supple.Data.Codec where

import Prelude

import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either)
import Supple.Data.Api (UserBucket(..))

decodeUserBucket :: String -> Either String UserBucket
decodeUserBucket str = do
  json <- jsonParser str
  o <- decodeJson json
  _ubUserId <- o .: "_ubUserId"
  _ubSku  <- o .: "_ubSku"
  _ubSvid  <- o .: "_ubSvid"
  _ubPrice  <- o .: "_ubPrice"
  _ubExpId  <- o .: "_ubExpId"
  _ubBucketId  <- o .: "_ubBucketId"
  pure $ UserBucket
   { _ubUserId , _ubSku , _ubSvid , _ubPrice , _ubExpId , _ubBucketId}
