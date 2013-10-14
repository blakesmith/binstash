{-# LANGUAGE DeriveGeneric #-}
module Binstash.Configuration where

import qualified Data.ByteString.Lazy as B
import GHC.Generics (Generic)
import System.IO (withFile, IOMode(WriteMode))
import Data.Aeson (ToJSON)
import Data.Aeson.Encode (encode)

data Credentials = Credentials { _token  :: String
                               , _secret :: String
                               } deriving (Eq, Show, Generic)

instance ToJSON Credentials

writeCredentials :: Credentials -> IO ()
writeCredentials creds = do
                 withFile "/Users/blake/.binstash" WriteMode $ \handle -> do
                 B.hPut handle $ encode creds

gatherCredentials :: IO ()
gatherCredentials = do
                  putStrLn "Enter your BinStash API Token: "
                  token <- getLine
                  putStrLn "Enter your BinStash API Secret: "
                  secret <- getLine
                  writeCredentials $ Credentials token secret

