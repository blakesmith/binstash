{-# LANGUAGE OverloadedStrings #-}
module Binstash.Cmd where

import qualified Data.ByteString.Lazy.Char8 as LB
import Control.Monad.Reader
import Binstash.Args
import Binstash.Configuration
import Binstash.Http

data Client = Client { _args :: Args
                     , _creds :: Credentials
                     } deriving (Show, Eq)

type ClientEnv = ReaderT Client IO

runCommand :: String -> ClientEnv (Either String String)
runCommand "list" = do
           creds <- asks _creds
           body <- liftIO $ httpLBS creds "http://api.binstash.com/repositories" "POST"
           return $ Right (LB.unpack body)

runCommand _ = return $ Left "Unknown command"

runClient :: ClientEnv (Either String String)
runClient = liftM command (asks _args) >>= runCommand

showResult :: (Either String String) -> String
showResult (Left err) = "Error: " ++ err
showResult (Right msg) = msg

run :: Client -> IO String
run client = liftM showResult (runReaderT runClient client)
