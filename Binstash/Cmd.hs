{-# LANGUAGE OverloadedStrings #-}
module Binstash.Cmd where

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

showPackage :: Maybe Package -> String
showPackage Nothing = "No package\n"
showPackage (Just package) = "Package added!\n" ++ show package

runCommand :: String -> ClientEnv (Either String String)
runCommand "list" = do
           httpArgs <- liftM3 HttpArgs (_secure `fmap` asks _args) (_domain `fmap` asks _args) (asks _creds)

           body <- liftIO $ httpLBS httpArgs "/repositories" "GET"
           return $ Right (listRepositories (decode body :: Maybe RepositoriesResponse))

runCommand "add" = do
           httpArgs <- liftM3 HttpArgs (_secure `fmap` asks _args) (_domain `fmap` asks _args) (asks _creds)

           f <- filename `fmap` asks _args
           dir <- directory_ `fmap` asks _args
           n <- name_ `fmap` asks _args
           body <- liftIO $ httpMultiForm httpArgs "/packages" [("directory", B.pack dir), ("name", B.pack n)] f
           return $ Right (showPackage (decode body :: Maybe Package))

runCommand _ = return $ Left "Unknown command"

runClient :: ClientEnv (Either String String)
runClient = liftM command (asks _args) >>= runCommand

showResult :: (Either String String) -> String
showResult (Left err) = "Error: " ++ err
showResult (Right msg) = msg

run :: Client -> IO String
run client = liftM showResult (runReaderT runClient client)
