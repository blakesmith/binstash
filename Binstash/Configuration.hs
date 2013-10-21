{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Binstash.Configuration where

import qualified Data.ByteString.Lazy as B
import GHC.Generics (Generic)
import System.Directory (getHomeDirectory, doesFileExist, createDirectoryIfMissing)
import System.IO (withFile, IOMode(WriteMode))
import Data.Aeson (ToJSON, FromJSON, decode)
import Data.Aeson.Encode (encode)
import Data.Maybe (fromJust)
import System.FilePath
import Control.Monad

data Credentials = Credentials { _token  :: String
                               , _secret :: String
                               } deriving (Eq, Show, Generic)

data HttpArgs = HttpArgs { _httpSecure :: Bool
                         , _httpDomain :: String
                         , _httpCredentials :: Credentials
                         } deriving (Eq, Show)

instance ToJSON Credentials
instance FromJSON Credentials

configDir :: IO FilePath
configDir = liftM (flip (</>) ".binstash") getHomeDirectory

credsLocation :: IO FilePath
credsLocation = liftM (flip (</>) "creds") configDir

readCredentials :: IO Credentials
readCredentials = liftM (fromJust . decode) $ credsLocation >>= B.readFile

writeCredentials :: Credentials -> IO Credentials
writeCredentials creds = configDir >>= dirCreate >> credsLocation >>= doWrite >> return creds
                 where dirCreate = createDirectoryIfMissing True
                       doWrite p = withFile p WriteMode encodeAndPut
                       encodeAndPut handle = B.hPut handle $ encode creds

gatherCredentials :: IO Credentials
gatherCredentials = liftM2 Credentials getToken getSecret >>= writeCredentials
                  where getToken = putStrLn "Enter your BinStash API Token: " >> getLine
                        getSecret = putStrLn "Enter your BinStash API Secret: " >> getLine

getCredentials :: IO Credentials
getCredentials = credsLocation >>= doesFileExist >>= creds
               where creds True = readCredentials
                     creds False = gatherCredentials
