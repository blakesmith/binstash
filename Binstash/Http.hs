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

urlFor :: HttpArgs -> String -> String
urlFor httpA route = getProtocol (_httpSecure httpA) ++ _httpDomain httpA ++ route
       where getProtocol :: Bool -> String
             getProtocol True = "https://"
             getProtocol False = "http://"

basicRequest :: Failure HttpException m => HttpArgs -> String -> m (Request m1)
basicRequest httpA route = liftM (applyBasicAuth user pass) $ (parseUrl . urlFor httpA) route
           where
                user = B.pack $ _token (_httpCredentials httpA)
                pass = B.pack $ _secret (_httpCredentials httpA)

httpLBS :: HttpArgs -> String -> Method -> IO LB.ByteString
httpLBS httpA route meth = do
           request <- basicRequest httpA route
           response <- withManager $ httpLbs request { method = meth }
           return (responseBody response)

httpMultiForm :: HttpArgs -> String -> [(String, B.ByteString)] -> FilePath -> IO LB.ByteString
httpMultiForm httpA route query filepath = do
           request <- basicRequest httpA route
           response <- withManager $ \m -> do flip httpLbs m =<< formRequest request
           return (responseBody response)
           where
                formRequest r = formDataBody ((parts query) ++ [partFileSource "package" filepath]) r
                parts qp = map (\q -> partBS (T.pack . fst $ q) (snd $ q)) qp