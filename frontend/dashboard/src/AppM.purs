module AppM where

import Prelude
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Effect.Aff (Aff)
import Type.Equality (class TypeEquals, from)
import Effect.Class.Console (log)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff.Class (class MonadAff)
import Capability.Navigate (class Navigate, navigate)

data LogLevel = Dev | Prod

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

type Env = { logLevel :: LogLevel}

newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance navigateAppM :: Navigate AppM where
  navigate _ = liftEffect <<< log $ "LEL"
