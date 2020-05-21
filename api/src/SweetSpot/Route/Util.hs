module SweetSpot.Route.Util where

import RIO
import Servant

internalServerErr = err500 {errBody = "Something went wrong"}

badRequestErr = err400 {errBody = "Bad request"}

unauthorizedErr = err403 {errBody = "Unauthorized"}

notFoundErr = err404 {errBody = "Not found"}

type Get303 (cts :: [*]) a =
  Verb 'GET 303 cts (Headers '[(Header "Location" Text)] a)
