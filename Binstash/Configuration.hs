{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Binstash.Configuration where

import qualified Data.ByteString.Lazy as B
import GHC.Generics (Generic)
import System.Directory (getHomeDirectory, doesFileExist)
import System.IO (withFile, IOMode(WriteMode))
import Data.Aeson (ToJSON, FromJSON, decode)
import Data.Aeson.Encode (encode)
import Data.Maybe (fromJust)
import System.FilePath
import Control.Monad

data Credentials = Credentials { _token  :: String
                               , _secret :: String
                               } deriving (Eq, Show, Generic)

instance ToJSON Credentials
instance FromJSON Credentials

homeDir :: IO FilePath
homeDir = liftM (flip combine ".binstash") getHomeDirectory

readCredentials :: IO Credentials
readCredentials = do
                body <- homeDir >>= B.readFile
                return $ (fromJust . decode) body

writeCredentials :: Credentials -> IO Credentials
writeCredentials creds = do
                 path <- homeDir
                 withFile path WriteMode $ \handle -> do
                 B.hPut handle $ encode creds
                 return creds

gatherCredentials :: IO Credentials
gatherCredentials = do
                  putStrLn "Enter your BinStash API Token: "
                  token <- getLine
                  putStrLn "Enter your BinStash API Secret: "
                  secret <- getLine
                  writeCredentials $ Credentials token secret

getCredentials :: IO Credentials
getCredentials = homeDir >>= doesFileExist >>= creds
               where creds True = readCredentials
                     creds False = gatherCredentials
