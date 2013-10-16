module Binstash.Cmd where

import Control.Monad.Reader
import Binstash.Args (Args)
import Binstash.Configuration (Credentials)

data Client = Client { _args :: Args
                     , _creds :: Credentials
                     } deriving (Show, Eq)

type ClientEnv = ReaderT Client IO


runClient :: ClientEnv ()
runClient = return ()

run :: Client -> IO ()
run client = runReaderT runClient client