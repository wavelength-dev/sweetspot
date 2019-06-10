{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Route.Util where

import Servant (err500, err400, err404, errBody)

internalServerErr = err500 {errBody = "Something went wrong"}

badRequestErr = err400 {errBody = "Bad request"}

notFoundErr = err404 {errBody = "Not found"}
