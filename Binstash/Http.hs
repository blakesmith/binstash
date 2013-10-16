{-# LANGUAGE OverloadedStrings #-}
module Binstash.Http where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Network.HTTP.Conduit
import Network.HTTP.Types (Method)
import Control.Monad
import Binstash.Configuration

httpLBS :: Credentials -> String -> Method -> IO LB.ByteString
httpLBS creds url meth = do
           request <- liftM (applyBasicAuth (user creds) (pass creds)) $ parseUrl url
           response <- withManager $ httpLbs request { method = meth }
           return (responseBody response)
           where
                user c = B.pack $ _token c
                pass c = B.pack $ _secret c
