{-# LANGUAGE OverloadedStrings #-}
module PXCrawler (someFunc, main) where



import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple
import System.Environment
import System.IO

import           Control.Exception          (try)

import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy.UTF8 as BLU

import Data.Aeson
import Data.Aeson.KeyMap as K
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Text as T

import Text.Regex.TDFA

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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

            let src = BLU.toString $ getResponseBody response
            let content = fromAttrib "content"  $ (extractContent $ parseTags $ src)!!0
            -- putStrLn $ content

            let json = (decode $ BLU.fromString content :: Maybe Value)
            case json of
                Nothing -> error "no json"
                Just  (Object obj) ->  do 
                    let m = K.toMap  obj
                    case m M.! "illust" of
                        Object m -> case (K.toMap m) M.! "94982825" of
                            Object m -> case (K.toMap m) M.! "createDate" of
                                String t -> do
                                    -- print (t::T.Text)
                                    let reg = "^[0-9]{2}([0-9]+)-([0-9]+)-([0-9]+)" :: T.Text
                                    print $ (t =~ reg :: (T.Text, T.Text, T.Text, [T.Text]))
                            
                            -- do
                            -- print $ (K.toMap obj) M.! "7484613"
                            -- print "done"
            where
                s = "<meta id=meta-preload-data>"::String
                extractContent = take 1 . dropWhile (~/= s)

            
          


    -- let picUrl = "https://i.pximg.net/img-original/img/2021/12/24/20/00/03/94982825_p0.jpg"

    -- request' <- parseRequest picUrl
    -- let request'' = setRequestHeader "User-Agent" [userAgent] request'
    -- let request = setRequestHeader "Referer" ["https://www.pixiv.net/"] request''
    -- response <- httpLBS request
    -- putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
    -- print $ getResponseHeader "Content-Type" response
    -- L8.writeFile (path++"/t.jpg") $ getResponseBody response
    -- print "done"
    