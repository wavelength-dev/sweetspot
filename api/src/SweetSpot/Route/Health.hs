{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module SweetSpot.Route.Health
  ( HealthAPI
  , healthHandler
  ) where

import Servant
import SweetSpot.AppM (ServerM)

type HealthAPI = "health" :> Get '[PlainText] NoContent

healthHandler :: ServerM NoContent
healthHandler = return NoContent
