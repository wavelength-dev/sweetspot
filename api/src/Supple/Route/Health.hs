{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Supple.Route.Health
  ( HealthAPI
  , healthHandler
  ) where

import Servant
import Supple.AppM (AppM)

type HealthAPI = "health" :> Get '[PlainText] NoContent

healthHandler :: AppM NoContent
healthHandler = return NoContent
