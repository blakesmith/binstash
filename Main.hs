import Control.Applicative (liftA2)
import Binstash.Args
import Binstash.Cmd
import Binstash.Configuration

main :: IO ()
main = liftA2 Client getArgs getCredentials >>= run >>= putStrLn
