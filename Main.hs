import Binstash.Configuration

main :: IO ()
main = getCredentials >>= (putStrLn . show)