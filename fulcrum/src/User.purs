module Fulcrum.User
  ( class LocalStorageAction
  , getItem
  , setItem
  , getUserId
  , setUserId
  , uidStorageKey
  , UserId(..)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem) as Storage

-- As we're making big changes to the schema we'll have users locally that are not in the database. We may have to handle that situation here or in the service. One approach could be always setting the userId that comes back on a getTestMaps request and having the service effectively execute the migration.

uidStorageKey :: String
uidStorageKey = "sweetspot__uid"

newtype UserId
  = UserId String

derive instance eqUserId :: Eq UserId

derive instance genericUserId :: Generic UserId _

derive instance newtypeUserId :: Newtype UserId _

class
  Monad m <= LocalStorageAction m where
  getItem :: String -> m (Maybe String)
  setItem :: String -> String -> m Unit

instance localStorageActionEffect :: LocalStorageAction Effect where
  getItem key = window >>= localStorage >>= Storage.getItem key
  setItem key value = window >>= localStorage >>= Storage.setItem key value

instance showUserId :: Show UserId where
  show (UserId userId) = "UserId=" <> userId

getUserId :: forall m. LocalStorageAction m => m (Maybe UserId)
getUserId = getItem uidStorageKey >>= map UserId >>> pure

setUserId :: forall m. LocalStorageAction m => UserId -> m Unit
setUserId userId = setItem uidStorageKey (unwrap userId)
