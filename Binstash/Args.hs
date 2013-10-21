{-# LANGUAGE DeriveDataTypeable #-}
module Binstash.Args where
import System.Console.CmdArgs 

data Args = Args { command :: String
                 , filename :: FilePath
                 , directory_ :: String
                 , name_ :: String
                 , domain_ :: String
                 , secure_ :: Bool
                 } deriving (Data, Typeable, Show, Eq)

version :: String
version = "0.0.1"

binArgs :: Args 
binArgs = Args { command = def &= args &= typ "COMMAND"
               , filename = def &= typFile &= help "Package file to upload"
               , directory_ = def &= name "d" &= typ "DIRECTORY" &= help "Package directory. Usually your username"
               , name_ = def &= typ "NAME" &= help "Repository name, run 'binstash list' to see your repositories"
               , domain_ = "api.binstash.com" &= typ "DOMAIN" &= help "Binstash API domain. Defaults to 'api.binstash.com'"
               , secure_ = False &= typ "SECURE" &= help "Should the client use https?"
               }
        &= program "binstash"
        &= summary ("BinStash command line client v" ++ version)

getArgs :: IO Args
getArgs = cmdArgs binArgs