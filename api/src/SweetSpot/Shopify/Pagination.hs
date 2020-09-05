{-# LANGUAGE TemplateHaskell #-}

module SweetSpot.Shopify.Pagination where

import Control.Lens (element, makeLenses, preview)
import RIO
import qualified RIO.List as L
import qualified RIO.Text as T
import Servant

newtype LinkHeader = LinkHeader Text
  deriving (FromHttpApiData)

newtype PageInfo = PageInfo Text
  deriving (Eq, Show, ToHttpApiData)

data Pagination
  = Pagination
      { _paginationPrevious :: !(Maybe PageInfo),
        _paginationNext :: !(Maybe PageInfo)
      }
  deriving (Eq, Show)

makeLenses ''Pagination

parseLinkHeader :: LinkHeader -> Pagination
parseLinkHeader (LinkHeader txt) =
  Pagination
    { _paginationNext = pageInfoByDirection "next",
      _paginationPrevious = pageInfoByDirection "previous"
    }
  where
    directions = T.split (== ',') txt
    pageInfoByDirection dir =
      directions
        & L.find (T.isInfixOf ("rel=" <> dir))
        >>= (T.split (== ';') >>> L.headMaybe)
        >>= ( T.strip
                >>> T.dropAround (\c -> c == '<' || c == '>')
                >>> T.split (== '?')
                >>> preview (element 1)
            )
        >>= (T.split (== '&') >>> L.find (T.isInfixOf "page_info="))
        >>= (T.split (== '=') >>> preview (element 1) >>> fmap PageInfo)
