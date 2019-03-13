module Supple.AppM where

import Prelude

import Control.Monad.Except.Trans (class MonadThrow, ExceptT, runExceptT, throwError)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, asks, runReaderT)
import Data.Either (Either(..))
import Effect.Aff (Aff, forkAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Supple.Capability (class AppCapability)
import Supple.Request (fetchUserBuckets, postLogPayload)
import Type.Equality (class TypeEquals, from)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

newtype ClientErr = ClientErr
  { message :: String
  , payload :: String
  }

type Env = { logLevel :: String }

newtype AppM a = AppM (ExceptT ClientErr (ReaderT Env Aff) a)

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadThrowAppM :: MonadThrow ClientErr AppM

runAppM :: forall a. Env -> AppM a -> Aff (Either ClientErr a)
runAppM env (AppM m) = runReaderT (runExceptT m) env

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance appCapabilityAppM :: AppCapability AppM where
  getUserId = liftEffect $ window >>= localStorage >>= getItem "supple_uid"

  setUserId uid = liftEffect $ window >>= localStorage >>= setItem "supple_uid" uid

  getUserBucket uid = do
    bucket <- liftAff $ fetchUserBuckets uid
    case bucket of
      Right b -> pure b
      Left err -> throwError (ClientErr { message: "Error fetching user bucket", payload: err })

  log msg = do
    -- Fork aff since we don't care about result
    _ <- liftAff $ forkAff $ postLogPayload msg
    pure unit
