{-# LANGUAGE OverloadedStrings #-}

module TypeBridges where

import Control.Applicative
import Language.PureScript.Bridge
import Language.PureScript.Bridge.PSTypes

suppleBridge :: BridgePart
suppleBridge = defaultBridge
  <|> priceBridge
  <|> skuBridge
  <|> expIdBridge
  <|> bucketIdBridge
  <|> svidBridge

priceBridge :: BridgePart
priceBridge = do
   typeName ^== "Price"
   return psNumber

skuBridge :: BridgePart
skuBridge = do
   typeName ^== "Sku"
   return psString

expIdBridge :: BridgePart
expIdBridge = do
   typeName ^== "ExpId"
   return psString

bucketIdBridge :: BridgePart
bucketIdBridge = do
   typeName ^== "BucketId"
   return psString

svidBridge :: BridgePart
svidBridge = do
   typeName ^== "Svid"
   return psString
