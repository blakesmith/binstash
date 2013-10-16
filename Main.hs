import Binstash.Configuration
import Binstash.Args

main :: IO ()
-- main = getCredentials >>= (putStrLn . show)

main = getArgs >>= print