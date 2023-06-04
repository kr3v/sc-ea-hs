{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Aeson
import GHC.Generics

data Column = Column {name :: String, typ :: String} deriving (Show, Eq, Generic)

data Command
  = CreateTable {name :: String, columns :: [Column]}
  | DropTable {name :: String}
  | Insert {name :: String, values :: [Value]}
  | Select {name :: String, columns :: [String]}
  | Update {name :: String, values :: [Value]}
  | Delete {name :: String}
  deriving (Show, Eq, Generic)

instance ToJSON Command
instance FromJSON Command