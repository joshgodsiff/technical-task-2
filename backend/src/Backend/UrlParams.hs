{-# LANGUAGE OverloadedStrings #-}

module Backend.UrlParams where

import Backend.Types hiding (ElementsResult (..))
import Data.Maybe ( fromMaybe )

data UrlParams = UrlParams OrderBy OrderDir CurrentPage PerPage
  deriving (Show, Eq)

-- There are probably much more robust ways to do this using some sort of parsing library,
-- if you actually need to handle a lot of query strings, or some complicated logic.
-- This does have the benefit of being very lightweight, though.
class Default a where
  def :: a

resolve :: Default a => Maybe a -> a
resolve = fromMaybe def

instance Default OrderBy where
  def = AtomicNumber

instance Default OrderDir where
  def = Asc

instance Default CurrentPage where
  def = CurrentPage 1

instance Default PerPage where
  def = PerPage 10

resolveParams :: Maybe OrderBy -> Maybe OrderDir -> Maybe CurrentPage -> Maybe PerPage -> UrlParams
resolveParams a b c d = UrlParams (resolve a) (resolve b) (resolve c) (resolve d)