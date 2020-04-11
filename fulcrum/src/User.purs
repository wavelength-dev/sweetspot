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
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)

-- As we're making big changes to the schema we'll have users locally that are not in the database. We may have to handle that situation here or in the service. One approach could be always setting the userId that comes back on a getTestMaps request and having the service effectively execute the migration.
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
findUserId = toMaybe >>> map UserId <$> readTrekkieToken
