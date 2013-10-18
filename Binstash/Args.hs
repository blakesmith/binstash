{-# LANGUAGE DeriveDataTypeable #-}
module Binstash.Args where
import System.Console.CmdArgs 

data Args = Args { command :: String
                 , filename :: FilePath
                 } deriving (Data, Typeable, Show, Eq)

version :: String
version = "0.0.1"

binArgs :: Args 
binArgs = Args { command = def &= args &= typ "COMMAND"
               , filename = def &= typFile &= help "Package file to upload"
               }
        &= program "binstash"
        &= summary ("BinStash command line client v" ++ version)

getArgs :: IO Args
getArgs = cmdArgs binArgs