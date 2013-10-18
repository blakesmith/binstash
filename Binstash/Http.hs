{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Binstash.Http where

import Control.Failure
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import Network.HTTP.Conduit
import Network.HTTP.Conduit.MultipartFormData
import Network.HTTP.Types (Method)
import Control.Monad
import Binstash.Configuration

proto :: String
domain :: String

proto = "http://"
domain = "api.binstash.com"

urlFor :: String -> String
urlFor route = proto ++ domain ++ route

basicRequest :: Failure HttpException m => Credentials -> String -> m (Request m1)
basicRequest creds route = liftM (applyBasicAuth (user creds) (pass creds)) $ (parseUrl . urlFor) route
           where
                user c = B.pack $ _token c
                pass c = B.pack $ _secret c

httpLBS :: Credentials -> String -> Method -> IO LB.ByteString
httpLBS creds route meth = do
           request <- basicRequest creds route
           response <- withManager $ httpLbs request { method = meth }
           return (responseBody response)

httpMultiForm :: Credentials -> String -> [(String, B.ByteString)] -> FilePath -> IO LB.ByteString
httpMultiForm creds route query filepath = do
           request <- basicRequest creds route
           response <- withManager $ \m -> do flip httpLbs m =<< formRequest request
           return (responseBody response)
           where
                formRequest r = formDataBody ((parts query) ++ [partFileSource "package" filepath]) r
                parts qp = map (\q -> partBS (T.pack . fst $ q) (snd $ q)) qp