{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Supple.Route.Static
  ( StaticAPI
  , staticHandler
  ) where

import Servant

type StaticAPI = "static" :> Raw

staticHandler = serveDirectoryFileServer "../dist/"
