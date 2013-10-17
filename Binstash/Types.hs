{-# LANGUAGE DeriveGeneric #-}
module Binstash.Types where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

data Repository = Repository { id :: Int
                             , directory :: String
                             , name :: String
                             , private :: Bool
                             , kind :: String
                             , architectures :: String
                             , distribution :: String
                             } deriving (Eq, Show, Generic)

instance FromJSON Repository

data RepositoriesResponse = RepositoriesResponse { repositories :: [Repository] } deriving (Eq, Show, Generic)

instance FromJSON RepositoriesResponse