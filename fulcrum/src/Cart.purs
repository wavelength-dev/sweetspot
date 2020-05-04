module Fulcrum.Cart
  ( CartToken
  , findCartToken
  , persistSentToken
  , hasCartTokenBeenSent
  ) where

import Prelude
import Control.Bind (bindFlipped)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut as Argonaut
import Data.Array as A
import Data.Either (Either(..), hush, note)
import Data.Maybe (Maybe, isJust, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String as S
import Effect (Effect)
import Fulcrum.Config as Config
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

newtype CartToken
  = CartToken String

derive instance newtypeCartToken :: Newtype CartToken _

derive instance eqCartToken :: Eq CartToken

instance encodeJsonCartToken :: EncodeJson CartToken where
  encodeJson = unwrap >>> Argonaut.encodeJson

instance decodeJsonCartToken :: DecodeJson CartToken where
  decodeJson json = CartToken <$> Argonaut.decodeJson json

foreign import readCookies :: Effect String

parseCartToken :: String -> Maybe CartToken
parseCartToken =
  S.split (S.Pattern ";")
    >>> map (S.split (S.Pattern "=") >>> map S.trim)
    >>> A.find (\arr -> maybe false (eq "cart") $ A.index arr 0)
    >>> bindFlipped (\arr -> CartToken <$> A.index arr 1)

findCartToken :: Effect (Maybe CartToken)
findCartToken = parseCartToken <$> readCookies

readStash :: Effect (Either String (Array CartToken))
readStash = do
  val <- window >>= localStorage >>= getItem Config.tokenStashKey
  pure
    $ note "Stash not found" val
    >>= Argonaut.jsonParser
    >>= Argonaut.decodeJson

hasCartTokenBeenSent :: CartToken -> Effect Boolean
hasCartTokenBeenSent token = do
  eStash <- readStash
  pure $ hush eStash >>= A.findIndex (eq token) # isJust

persistSentToken :: CartToken -> Effect Unit
persistSentToken token = do
  eStash <- readStash
  case eStash of
    Left _ -> storeStash [ token ]
    Right arr -> storeStash $ A.snoc arr token
  where
  storeStash :: Array CartToken -> Effect Unit
  storeStash tokenArr = do
    let
      payload = Argonaut.stringify $ Argonaut.encodeJson tokenArr
    window >>= localStorage >>= setItem Config.tokenStashKey payload
