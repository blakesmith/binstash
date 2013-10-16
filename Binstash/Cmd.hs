module Binstash.Cmd where

import Control.Monad.Reader
import Binstash.Args (Args)
import Binstash.Configuration (Credentials)

data Client = Client { _args :: Args
                     , _creds :: Credentials
                     } deriving (Show, Eq)

type ClientEnv = ReaderT Client IO


runClient :: ClientEnv (Either String String)
runClient = return $ Right "Success!"

showResult :: (Either String String) -> String
showResult (Left err) = "Error: " ++ err
showResult (Right msg) = msg

run :: Client -> IO String
run client = liftM showResult (runReaderT runClient client)
