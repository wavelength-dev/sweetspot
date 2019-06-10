{-# LANGUAGE OverloadedStrings #-}

module TypeBridges where

import Control.Applicative
import Language.PureScript.Bridge
import Language.PureScript.Bridge.PSTypes

sweetspotBridge :: BridgePart
sweetspotBridge = defaultBridge
  <|> (typeName ^== "Price" >> return psNumber)
  <|> (typeName ^== "Sku" >> return psString)
  <|> (typeName ^== "ExpId" >> return psNumber)
  <|> (typeName ^== "BucketId" >> return psNumber)
  <|> (typeName ^== "Svid" >> return psNumber)
  <|> (typeName ^== "Pid" >> return psNumber)
  <|> (typeName ^== "Image" >> return psString)
  <|> (typeName ^== "CampaignId" >> return psString)
  <|> (typeName ^== "UserId" >> return psNumber)
