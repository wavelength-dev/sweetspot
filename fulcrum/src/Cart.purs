module Fulcrum.Cart
  ( CartToken
  , findCartToken
  )
where

import Prelude

import Control.Bind (bindFlipped)
import Data.Argonaut (class EncodeJson)
import Data.Argonaut (encodeJson) as Argonaut
import Data.Array as A
import Data.Maybe (Maybe, isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.String as S
import Effect (Effect)

newtype CartToken = CartToken String

derive instance newtypeUserId :: Newtype CartToken _

instance encodeJsonUserId :: EncodeJson CartToken where
  encodeJson = unwrap >>> Argonaut.encodeJson

foreign import readCookies :: Effect String

parseCartToken :: String -> Maybe CartToken
parseCartToken = S.split (S.Pattern ";")
  >>> map (S.split (S.Pattern "=") >>> map S.trim)
  >>> A.find (\arr -> isJust $ eq "cart" <$> A.index arr 0)
  >>> bindFlipped (\arr -> CartToken <$> A.index arr 1)

findCartToken :: Effect (Maybe CartToken)
findCartToken = parseCartToken <$> readCookies
