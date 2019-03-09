{-# LANGUAGE OverloadedStrings #-}

module Supple.Route.Util where

import Servant (err500, errBody)

internalServerErr = err500 {errBody = "Something went wrong"}
