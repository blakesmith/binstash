{-# LANGUAGE OverloadedStrings #-}
module Binstash.Http where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import Network.HTTP.Conduit
import Network.HTTP.Conduit.MultipartFormData
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

httpMultiForm :: Credentials -> String -> [(String, B.ByteString)] -> FilePath -> IO LB.ByteString
httpMultiForm creds url query filepath = do
           request <- liftM (applyBasicAuth (user creds) (pass creds)) $ parseUrl url
           response <- withManager $ \m -> do flip httpLbs m =<< formRequest request
           return (responseBody response)
           where
                user c = B.pack $ _token c
                pass c = B.pack $ _secret c
                formRequest r = formDataBody ((parts query) ++ [partFileSource "package" filepath]) r
                parts qp = map (\q -> partBS (T.pack . fst $ q) (snd $ q)) qp