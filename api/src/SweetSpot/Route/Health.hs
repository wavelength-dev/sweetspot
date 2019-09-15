{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module SweetSpot.Route.Health
  ( HealthAPI
  , healthHandler
  ) where

import Servant

type HealthAPI = "health" :> Get '[PlainText] NoContent

healthHandler :: (Monad m) => m NoContent
healthHandler = return NoContent
