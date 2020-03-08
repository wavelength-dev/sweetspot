{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module SweetSpot.Route.Index
  ( IndexRoute
  , indexHandler
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Encoding as TE
import Network.HTTP.Media ((//), (/:))
import Servant
import Web.Cookie (defaultSetCookie, SetCookie(..))
import SweetSpot.AppM (AppM(..), ServerM)
import SweetSpot.Data.Common
import SweetSpot.Route.Util (badRequestErr)

data HTML

newtype RawHTML = RawHTML { unRaw :: BS.ByteString }

instance MimeRender HTML RawHTML where
  mimeRender _ =  unRaw

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

type HTMLWithCookie = Headers '[Header "Set-Cookie" SetCookie] RawHTML

type IndexRoute = "dashboard"
  :> QueryParam "shop" ShopDomain
  :> Get '[HTML] HTMLWithCookie

indexHandler :: Maybe ShopDomain -> ServerM HTMLWithCookie
indexHandler (Just (ShopDomain domain)) = runAppM $
  addHeader cookie . RawHTML <$> liftIO (BS.readFile "../dist/index.html")
  where
    cookie = defaultSetCookie
      { setCookieName = "sweetspotShopOrigin"
      , setCookieValue = TE.encodeUtf8 domain
      }

indexHandler _ = throwError badRequestErr
