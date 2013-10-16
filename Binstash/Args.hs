{-# LANGUAGE DeriveDataTypeable #-}
module Binstash.Args where
import System.Console.CmdArgs 

data Args = Args { command :: String } deriving (Data, Typeable, Show, Eq)

binArgs :: Args 
binArgs = Args { command = def &= args &= typ "COMMAND"}
        &= program "binstash"
        &= summary "BinStash command line client"

getArgs :: IO Args
getArgs = cmdArgs binArgs