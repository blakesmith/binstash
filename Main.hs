import Binstash.Configuration
import Binstash.Args

data Client = Client { _args :: Args
                     , _creds :: Credentials
                     } deriving (Show, Eq)

main :: IO ()
main = do
     args <- getArgs
     creds <- getCredentials
     putStrLn . show $ Client args creds
