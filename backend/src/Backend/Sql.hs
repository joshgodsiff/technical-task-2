{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Sql where

import Backend.UrlParams
import Backend.Types hiding (ElementsResult (..))
import Database.SQLite.Simple ( NamedParam(..), Query )

countQuery :: Query
countQuery = "SELECT COUNT (*) FROM elements"

elementsQueryParams :: CurrentPage -> PerPage -> [NamedParam]
elementsQueryParams (CurrentPage currentPage) (PerPage perPage) =
  [ ":offset" := (currentPage - 1) * perPage
  , ":limit"  := perPage
  ]

elementsQuery :: OrderBy -> OrderDir -> Query
elementsQuery orderBy_ orderDir_ =
  let
    orderBy = case orderBy_ of
      AtomicNumber -> "atomic_number"
      ElementName  -> "element"
      Symbol       -> "symbol"
      Type         -> "type"
    orderDir = case orderDir_ of
      Asc  -> "asc"
      Desc -> "desc"
  -- SQLite doesn't let you perform query substitution of things like the arguments to ORDER BY, so we have to throw those in manually.
  -- That is also why we're round-tripping them through an internal type, to prevent SQL injection vulnerabilities.
  in "SELECT atomic_number, element, symbol, type FROM elements ORDER BY " <> orderBy <> " " <> orderDir <> " LIMIT :limit OFFSET :offset"