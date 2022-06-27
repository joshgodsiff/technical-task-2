{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Servant where

import Servant
import Database.SQLite.Simple
import Control.Monad.IO.Class (liftIO, MonadIO)
import Backend.Types
import Backend.Sql
import Backend.UrlParams

type ElementsApi = 
  "elements" 
    :> QueryParam "orderBy" OrderBy
    :> QueryParam "orderDir" OrderDir
    :> QueryParam "currentPage" CurrentPage
    :> QueryParam "perPage" PerPage
    :> Get '[JSON] ElementsResult

elementsHandler :: Maybe OrderBy -> Maybe OrderDir -> Maybe CurrentPage -> Maybe PerPage -> Handler ElementsResult
elementsHandler orderBy_ orderDir_ currentPage_ perPage_ = do
  let UrlParams orderBy orderDir currentPage perPage = resolveParams orderBy_ orderDir_ currentPage_ perPage_
  conn <- liftIO $ open "../db/elements.db"
  -- In a high-traffic scenario, we might want to do these calls concurrently,
  -- or roll them into a single transaction query so we only make one call to the DB.
  (cs :: [CountResult])  <- liftIO $ query_ conn countQuery
  (results :: [Element]) <- liftIO $ queryNamed conn (elementsQuery orderBy orderDir) (elementsQueryParams currentPage perPage)
  let 
    total = case cs of
      [c] -> c
      _ -> CountResult 0
  pure $ ElementsResult 
        { total = total
        , data_ = results
        }

app :: Application
app = serve (Proxy :: Proxy ElementsApi) elementsHandler