{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Types where

import Data.Text
import GHC.Generics (Generic)
import Data.Aeson
import Database.SQLite.Simple
import Web.HttpApiData

elementEncodingOptions :: Options
elementEncodingOptions = defaultOptions { fieldLabelModifier = renameKeywords }

renameKeywords :: String -> String
renameKeywords "type_" = "type"
renameKeywords "data_" = "data"
renameKeywords a = a

data Element = Element
  { atomic_number :: Int
  , element :: Text
  , symbol :: Text
  , type_ :: Text
  } deriving (Show, Eq, Generic, FromRow)

instance ToJSON Element where
  toJSON = genericToJSON elementEncodingOptions

newtype CountResult = CountResult { unCountResult :: Int }
  deriving (Eq, Show, Generic)
  deriving ToJSON via Int
  deriving anyclass FromRow

data ElementsResult = ElementsResult
  { total :: CountResult
  -- These are returned by the example JS code, but they're not used by the frontend, 
  -- so I don't see a particular need to support them for this project.
  -- , perPage :: Int
  -- , offset :: Int
  -- , to :: Int
  -- , lastPage :: Int
  -- , currentPage :: Int
  -- , from :: Int
  , data_ :: [Element]
  } deriving (Show, Eq, Generic)

instance ToJSON ElementsResult where
  toJSON = genericToJSON elementEncodingOptions

data OrderBy = AtomicNumber | ElementName | Symbol | Type
  deriving (Show, Eq)

newtype CurrentPage = CurrentPage { unCurrentPage :: Int }
  deriving (Show, Eq, FromHttpApiData) via Int

newtype PerPage = PerPage { unPerPage :: Int }
  deriving (Show, Eq, FromHttpApiData) via Int

data OrderDir = Asc | Desc
  deriving (Show, Eq)

instance FromHttpApiData OrderDir where
  parseQueryParam t = case t of
    "asc"  -> Right Asc
    "desc" -> Right Desc
    _      -> Left "Unknown value for orderDir, expected one of 'asc' or 'desc'."

instance FromHttpApiData OrderBy where
  parseQueryParam t = case t of
    "atomic_number" -> Right AtomicNumber
    "element"       -> Right ElementName
    "symbol"        -> Right Symbol
    "type"          -> Right Type
    _               -> Left "Unknown column for orderBy, expected one of 'atomic_number', 'element', 'symbol', or 'type'"