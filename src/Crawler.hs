{-# LANGUAGE OverloadedStrings #-}

module Crawler (main) where

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as LBS

import Network.HTTP.Simple (Response, HttpException, parseRequest, getResponseBody, httpLBS, setRequestHeader, setRequestHeaders, getResponseHeader)
import Data.Traversable (forM)

import           Control.Exception          (try)

import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy.UTF8 as LBU

import Data.Aeson
import Data.Aeson.KeyMap as K
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)

import Text.Regex.TDFA
-- import Data.ByteString (ByteString)

userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) \
            \Chrome/96.0.4664.110 Safari/537.36 Edg/96.0.1054.62"
referer = "https://www.pixiv.net/"
artworksBase = "https://www.pixiv.net/artworks/"
path = "/mnt/c/Users/33160/Desktop"

requestUrl :: String -> Int -> IO (Either HttpException (Response LBS.ByteString))
requestUrl url retry = do
    request' <- parseRequest url
    let request = setRequestHeaders [("User-Agent", userAgent), ("Referer", referer)] $ request'
    eresponse <- try $ httpLBS request
    if retry == 0 then return eresponse else
        case eresponse :: Either HttpException (Response LBS.ByteString) of
            Left e -> requestUrl url $ retry - 1
            Right response -> return (Right response)

data Artwork = Artwork {artist::Text, date::Text, title::Text, id::Text}

extractField :: Value -> [Key] -> Text
extractField (Object obj) fields =   do 
    let m = K.toMap  obj
    case fields of
        [x] -> case m M.! x of
            String s -> s
        x:xs -> case m M.! x of
                o -> extractField o xs



main :: IO ()
main = do
    args <- getArgs
    let id = args !! 0
    
    eresponse <- requestUrl  (artworksBase ++ id ) 1

    case eresponse of
        Left e -> print (e :: HttpException)
        Right response -> do
            print $ getResponseHeader "Content-Type" response
            LBS.writeFile (path++"/t.html") $ getResponseBody response

            let src = LBU.toString $ getResponseBody response
            let content = fromAttrib "content"  $ head $ extractContent $ parseTags src
                

            let json = (decode $ LBU.fromString content :: Maybe Value)
            case json of
                Nothing -> error "no json"
                Just v -> print m
                    where
                        t = extractField v ["illust", "94982825", "createDate"]
                        reg = "^[0-9]{2}([0-9]+)-([0-9]+)-([0-9]+)" :: T.Text
                        m = (t =~ reg :: (T.Text, T.Text, T.Text, [T.Text]))
            print "done"
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
    