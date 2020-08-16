module Fulcrum.User
  ( UserId(..)
  , findUserId
  ) where

import Prelude
import Data.Argonaut (class EncodeJson)
import Data.Argonaut (encodeJson) as Argonaut
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Nullable (Nullable)
import Data.Nullable (toMaybe) as Nullable
import Effect (Effect)

-- | UserIds are id's assigned to users within the SweetSpot system.
newtype UserId
  = UserId String

derive instance eqUserId :: Eq UserId

derive instance genericUserId :: Generic UserId _

derive instance newtypeUserId :: Newtype UserId _

instance encodeJsonUserId :: EncodeJson UserId where
  encodeJson = unwrap >>> Argonaut.encodeJson

instance showUserId :: Show UserId where
  show (UserId userId) = "UserId=" <> userId

foreign import readTrekkieToken :: Effect (Nullable String)

findUserId :: Effect (Maybe UserId)
findUserId = readTrekkieToken <#> Nullable.toMaybe >>> map UserId
