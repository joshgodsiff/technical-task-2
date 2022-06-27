{-# LANGUAGE OverloadedStrings #-}

module Backend.UrlParams where

import Backend.Types hiding (ElementsResult (..))
import Data.Maybe

data UrlParams = UrlParams OrderBy OrderDir CurrentPage PerPage
  deriving (Show, Eq)

class Resolve a where
  resolve :: Maybe a -> a

instance Resolve OrderBy where
  resolve = fromMaybe AtomicNumber

instance Resolve OrderDir where
  resolve = fromMaybe Asc

instance Resolve CurrentPage where
  resolve = fromMaybe . CurrentPage $ 1

instance Resolve PerPage where
  resolve = fromMaybe . PerPage $ 10

resolveParams :: Maybe OrderBy -> Maybe OrderDir -> Maybe CurrentPage -> Maybe PerPage -> UrlParams
resolveParams a b c d = UrlParams (resolve a) (resolve b) (resolve c) (resolve d)