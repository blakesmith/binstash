{-# LANGUAGE OverloadedStrings #-}
module Binstash.Cmd where

import Data.Aeson (decode)
import Data.List (intersperse)
import Control.Monad.Reader
import Binstash.Args
import Binstash.Configuration
import Binstash.Http
import Binstash.Types

data Client = Client { _args :: Args
                     , _creds :: Credentials
                     } deriving (Show, Eq)

type ClientEnv = ReaderT Client IO

listRepositories :: Maybe RepositoriesResponse -> String
listRepositories resp = case resp of
                  Just res -> joinA "\n" $ repos res
                  Nothing -> "No repositories"
    where repos r = map line $ zip [1..] (repositories r)
          line :: (Int, Repository) -> String
          line t = show (fst t) ++ ". " ++ (directory (snd t) ++ "/" ++ name (snd t))
          joinA delim l = concat (intersperse delim l)

runCommand :: String -> ClientEnv (Either String String)
runCommand "list" = do
           creds <- asks _creds
           body <- liftIO $ httpLBS creds "http://api.binstash.com/repositories" "POST"
           return $ Right (listRepositories (decode body :: Maybe RepositoriesResponse))

runCommand _ = return $ Left "Unknown command"

runClient :: ClientEnv (Either String String)
runClient = liftM command (asks _args) >>= runCommand

showResult :: (Either String String) -> String
showResult (Left err) = "Error: " ++ err
showResult (Right msg) = msg

run :: Client -> IO String
run client = liftM showResult (runReaderT runClient client)
