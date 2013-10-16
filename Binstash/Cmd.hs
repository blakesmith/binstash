module Binstash.Cmd where

import Control.Monad.Reader
import Binstash.Args
import Binstash.Configuration (Credentials)

data Client = Client { _args :: Args
                     , _creds :: Credentials
                     } deriving (Show, Eq)

type ClientEnv = ReaderT Client IO

runCommand :: String -> ClientEnv (Either String String)
runCommand "list" = return $ Right "Success!"
runCommand _ = return $ Left "Unknown command"

runClient :: ClientEnv (Either String String)
runClient = liftM command (asks _args) >>= runCommand

showResult :: (Either String String) -> String
showResult (Left err) = "Error: " ++ err
showResult (Right msg) = msg

run :: Client -> IO String
run client = liftM showResult (runReaderT runClient client)
