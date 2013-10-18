{-# LANGUAGE OverloadedStrings #-}
module Binstash.Cmd where

import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B
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
listRepositories resp = case repositories `fmap` resp of
                  Just res -> joinA "\n" $ repos res
                  Nothing -> "No repositories"
    where repos r = map line $ zip [1..] r
          line :: (Int, Repository) -> String
          line t = show (fst t) ++ ". " ++ (directory (snd t) ++ "/" ++ name (snd t))
          joinA delim l = concat (intersperse delim l)

runCommand :: String -> ClientEnv (Either String String)
runCommand "list" = do
           creds <- asks _creds
           body <- liftIO $ httpLBS creds "http://api.binstash.com/repositories" "GET"
           return $ Right (listRepositories (decode body :: Maybe RepositoriesResponse))

runCommand "add" = do
           creds <- asks _creds
           f <- liftM filename (asks _args)
           dir <- liftM directory_ (asks _args)
           n <- liftM name_ (asks _args)
           body <- liftIO $ httpMultiForm creds "http://api.binstash.com/packages" [("directory", B.pack dir), ("name", B.pack n)] f
           return $ (Right . LB.unpack) body

runCommand _ = return $ Left "Unknown command"

runClient :: ClientEnv (Either String String)
runClient = liftM command (asks _args) >>= runCommand

showResult :: (Either String String) -> String
showResult (Left err) = "Error: " ++ err
showResult (Right msg) = msg

run :: Client -> IO String
run client = liftM showResult (runReaderT runClient client)
