{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module SweetSpot.Route.Util where

import           Data.ByteString                ( ByteString )
import           Data.Text                      ( Text )
import           Network.Wai                    ( queryString )
import           Servant
import           Servant.Server.Internal        ( passToServer
                                                , hoistServerWithContext
                                                )


internalServerErr = err500 { errBody = "Something went wrong" }

badRequestErr = err400 { errBody = "Bad request" }

notFoundErr = err404 { errBody = "Not found" }

type Get303 (cts :: [*]) a
        = Verb 'GET 303 cts (Headers '[(Header "Location" Text)] a)
