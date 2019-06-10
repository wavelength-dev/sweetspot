{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module SweetSpot.Route.Health
  ( HealthAPI
  , healthHandler
  ) where

import Servant
import SweetSpot.AppM (AppM)

type HealthAPI = "health" :> Get '[PlainText] NoContent

healthHandler :: AppM NoContent
healthHandler = return NoContent
