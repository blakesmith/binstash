{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Binstash.Types where

import Control.Applicative ((<*>), (<$>))
import Control.Monad (mzero)
import Data.Aeson
import Data.List (intersperse)
import GHC.Generics (Generic)

data Repository = Repository { id :: Int
                             , directory :: String
                             , name :: String
                             , private :: Bool
                             , kind :: String
                             , architectures :: String
                             , distribution :: String
                             } deriving (Eq, Show, Generic)

instance FromJSON Repository

data Package = Package { packageId :: Int
                       , packageName :: String
                       , version :: String
                       , description :: String
                       , section :: String
                       , packageType :: String
                       , architecture :: String
                       , active :: Bool
                       } deriving (Eq, Generic)

instance FromJSON Package where
    parseJSON (Object v) = Package <$>
                           v .: "id"           <*>
                           v .: "name"         <*>
                           v .: "version"      <*>
                           v .: "description"  <*>
                           v .: "section"      <*>
                           v .: "packageType"  <*>
                           v .: "architecture" <*>
                           v .: "active"
    parseJSON _ = mzero

instance Show Package where
    show p = concat (intersperse "\n" [ "Name: " ++ (packageName p)
                                      , "Version: " ++ (version p)
                                      , "Description: " ++ (description p)
                                      , "Section: " ++ (packageType p)
                                      , "Type: " ++ (packageType p)
                                      , "Architecture: " ++ (architecture p)
                                      , "Active: " ++ show (active p)
                                      ])

data RepositoriesResponse = RepositoriesResponse { repositories :: [Repository] } deriving (Eq, Show, Generic)

instance FromJSON RepositoriesResponse