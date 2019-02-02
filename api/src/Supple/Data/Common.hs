{-# LANGUAGE OverloadedStrings #-}

module Supple.Data.Common where

import Data.Scientific (Scientific)

type Price = Scientific

data EventType
  = View
  | Tag
