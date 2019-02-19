module Supple.Component.Form.Validation where

import Prelude

import Formless as F
import Data.Number (fromString)
import Data.String (null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

data Error = Required

isNonEmpty :: forall form m. Monad m => F.Validation form m Error String String
isNonEmpty = F.hoistFnE_ $ \str ->
  if null str
      then Left Required
      else Right str

isNumber :: forall form m. Monad m => F.Validation form m Error String Number
isNumber = F.hoistFnE_ $ \str ->
  case fromString str of
    Just n -> Right n
    Nothing -> Left Required

isGtZero :: forall form m. Monad m => F.Validation form m Error Number Number
isGtZero = F.hoistFnE_ $ \n ->
  if n > 0.0
     then Right n
     else Left Required
