import Binstash.Args
import Binstash.Cmd
import Binstash.Configuration

main :: IO ()
main = do
     args <- getArgs
     creds <- getCredentials
     result <- run $ Client args creds
     putStrLn result
