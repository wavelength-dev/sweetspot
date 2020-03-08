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
import SweetSpot.Database.Queries.Install (InstallDB(..))
import SweetSpot.Route.Util (badRequestErr)

data HTML

newtype RawHTML = RawHTML { unRaw :: BS.ByteString }

instance MimeRender HTML RawHTML where
  mimeRender _ =  unRaw

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

type HTMLWithCookie = Headers '[Header "Set-Cookie" SetCookie] RawHTML

type IndexRoute = "dashboard" :> "index.html"
  :> QueryParam "shop" ShopDomain
  :> QueryParam "timestamp" Timestamp
  :> QueryParam "hmac" HMAC'
  :> Get '[HTML] HTMLWithCookie

indexHandler
  :: Maybe ShopDomain
  -> Maybe Timestamp
  -> Maybe HMAC'
  -> ServerM HTMLWithCookie
indexHandler (Just domain) (Just (Timestamp ts)) (Just (HMAC' hmac)) =
  runAppM $ do
    mToken <- getOAuthToken domain
    let ShopDomain txtDomain = domain
    case mToken of
      Just _ -> addHeader cookie . RawHTML <$> liftIO (BS.readFile "../dist/index.html")
        where
          cookie = defaultSetCookie
            { setCookieName = "sweetspotShopOrigin"
            , setCookieValue = TE.encodeUtf8 txtDomain
            }
      Nothing -> throwError $ err302 { errHeaders = [("Location", redirectPath)] }
        where
          redirectPath = TE.encodeUtf8 $ "/api/oauth/install?shop=" <> txtDomain
            <> "&timestamp=" <> ts
            <> "&hmac=" <> hmac

indexHandler _ _ _ = throwError badRequestErr
