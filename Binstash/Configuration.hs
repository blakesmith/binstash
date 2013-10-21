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

configLocation :: IO FilePath
configLocation = liftM (flip combine ".binstash") getHomeDirectory

readCredentials :: IO Credentials
readCredentials = liftM (fromJust . decode) $ configLocation >>= B.readFile

writeCredentials :: Credentials -> IO Credentials
writeCredentials creds = configLocation >>= doWrite >> return creds
                 where doWrite p = withFile p WriteMode encodeAndPut
                       encodeAndPut handle = B.hPut handle $ encode creds

gatherCredentials :: IO Credentials
gatherCredentials = liftM2 Credentials getToken getSecret >>= writeCredentials
                  where getToken = putStrLn "Enter your BinStash API Token: " >> getLine
                        getSecret = putStrLn "Enter your BinStash API Secret: " >> getLine

getCredentials :: IO Credentials
getCredentials = configLocation >>= doesFileExist >>= creds
               where creds True = readCredentials
                     creds False = gatherCredentials
