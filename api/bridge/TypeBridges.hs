{-# LANGUAGE OverloadedStrings #-}

module TypeBridges where

import Control.Applicative
import Language.PureScript.Bridge
import Language.PureScript.Bridge.PSTypes

sweetspotBridge :: BridgePart
sweetspotBridge = defaultBridge
  <|> (typeName ^== "Price" >> return psNumber)
  <|> (typeName ^== "Sku" >> return psString)
  <|> (typeName ^== "Svid" >> return psString)
  <|> (typeName ^== "Pid" >> return psString)
  <|> (typeName ^== "CampaignId" >> return psString)
  <|> (typeName ^== "LocalTime" >> return psString)
