{-# LANGUAGE DeriveDataTypeable #-}
module Binstash.Args where
import System.Console.CmdArgs 

data Args = Args { command :: String } deriving (Data, Typeable, Show, Eq)

version :: String
version = "0.0.1"

binArgs :: Args 
binArgs = Args { command = def &= args &= typ "COMMAND"}
        &= program "binstash"
        &= summary ("BinStash command line client (" ++ version ++ ")")

getArgs :: IO Args
getArgs = cmdArgs binArgs