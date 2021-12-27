{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple
import System.Environment
import System.IO

import           Control.Exception          (try)

userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) \
            \Chrome/96.0.4664.110 Safari/537.36 Edg/96.0.1054.62"
artworksBase = "https://www.pixiv.net/artworks/"
path = "/mnt/c/Users/33160/Desktop"


main :: IO ()
main = do
    args <- getArgs
    let id = args !! 0
    
    request' <- parseRequest $ artworksBase ++ id
    let request = setRequestHeader "User-Agent" [userAgent] request'
    eresponse <- try $ httpLBS request

    case eresponse of
        Left e -> print (e :: HttpException)
        Right response -> do
            putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
            print $ getResponseHeader "Content-Type" response
            L8.writeFile (path++"/t.html") $ getResponseBody response


    let picUrl = "https://i.pximg.net/img-original/img/2021/12/24/20/00/03/94982825_p0.jpg"

    request' <- parseRequest picUrl
    let request'' = setRequestHeader "User-Agent" [userAgent] request'
    let request = setRequestHeader "Referer" ["https://www.pixiv.net/"] request''
    response <- httpLBS request
    putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.writeFile (path++"/t.jpg") $ getResponseBody response
    print "done"
    