import Binstash.Configuration

main :: IO ()
main = gatherCredentials >>= (putStrLn . show)