module Fulcrum.EstablishedTitles where

import Prelude
import Effect (Effect)
import Fulcrum.Site (readHostname)

isCurrentSite :: Effect Boolean
isCurrentSite = readHostname <#> (==) "establishedtitles.com"
