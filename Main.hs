import Binstash.Configuration
import Binstash.Args
import Control.Monad.Reader

data Client = Client { _args :: Args
                     , _creds :: Credentials
                     } deriving (Show, Eq)

type ClientEnv = ReaderT Client IO

runClient :: ClientEnv ()
runClient = return ()

main :: IO ()
main = do
     args <- getArgs
     creds <- getCredentials
     result <- runReaderT runClient (Client args creds)
     putStrLn . show $ result
