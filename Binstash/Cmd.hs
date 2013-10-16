{-# LANGUAGE OverloadedStrings #-}
module Binstash.Cmd where

import qualified Data.ByteString.Char8 as B
import Data.Conduit           (($$+-))
import Control.Monad.Reader
import Network.HTTP.Conduit
import Binstash.Args
import Binstash.Configuration

data Client = Client { _args :: Args
                     , _creds :: Credentials
                     } deriving (Show, Eq)

type ClientEnv = ReaderT Client IO

runCommand :: String -> ClientEnv (Either String String)
runCommand "list" = do
           creds <- asks _creds
           request <- liftM (applyBasicAuth (user creds) (pass creds)) $ parseUrl "http://api.binstash.com/repositories"
           withManager $ \manager -> do
               response <- http request { method = "POST" } manager
               responseBody response $$+- return ()
               return $ Right ((show . responseStatus) response)
           where
                user c = B.pack $ _token c
                pass c = B.pack $ _secret c

runCommand _ = return $ Left "Unknown command"

runClient :: ClientEnv (Either String String)
runClient = liftM command (asks _args) >>= runCommand

showResult :: (Either String String) -> String
showResult (Left err) = "Error: " ++ err
showResult (Right msg) = msg

run :: Client -> IO String
run client = liftM showResult (runReaderT runClient client)
