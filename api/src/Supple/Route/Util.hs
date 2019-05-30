{-# LANGUAGE OverloadedStrings #-}

module Supple.Route.Util where

import Servant (err500, err400, errBody)

internalServerErr = err500 {errBody = "Something went wrong"}

badRequestErr = err400 {errBody = "Bad request"}
