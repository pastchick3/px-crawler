{-# LANGUAGE OverloadedStrings #-}

module Crawler (crawlArtwork) where

import Control.Exception (try)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Scientific as Scientific
import Data.Text (unpack)
import Network.HTTP.Simple (HttpException, Response, getResponseBody, getResponseStatusCode, httpLBS, parseRequest, setRequestHeaders)
import System.Directory (createDirectory)
import Text.HTML.TagSoup (fromAttrib, parseTags, (~/=))
import Text.Regex.TDFA ((=~))

retry :: Int
retry = 1

path :: String
path = "./"

userAgent :: BS.ByteString
userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/96.0.4664.110 Safari/537.36 Edg/96.0.1054.62"

data Artwork = Artwork {artist :: String, date :: String, title :: String, contentId :: String}

instance Show Artwork where
  show (Artwork artist date title contentId) = "[" ++ artist ++ "] [" ++ date ++ "] " ++ title ++ " (" ++ contentId ++ ")"

crawlArtwork :: String -> IO ()
crawlArtwork contentId = do
  putStrLn ("Crawl " ++ contentId)
  response <- requestUrl ("https://www.pixiv.net/artworks/" ++ contentId) retry
  case response of
    Left _ -> putStrLn ("    Fail to crawl " ++ contentId)
    Right response -> case getResponseStatusCode response of
      200 -> do
        let (artwork, imageBase, imageExt, pageCount) = extractArtwork contentId response
        createDirectory $ path ++ show artwork
        mapM_ (crawlImage contentId artwork imageBase imageExt) [0 .. pageCount - 1]
      _ -> putStrLn ("    Fail to crawl " ++ contentId)

requestUrl :: String -> Int -> IO (Either HttpException (Response ByteString))
requestUrl url retry = do
  request <- parseRequest url
  let headers = [("User-Agent", userAgent), ("Referer", "https://www.pixiv.net/")]
  response <- try $ httpLBS $ setRequestHeaders headers request
  case (retry, response) of
    (retry, Left _) | retry > 0 -> requestUrl url (retry - 1)
    (_, response) -> return response

extractArtwork :: String -> Response ByteString -> (Artwork, String, String, Int)
extractArtwork contentId response = (artwork, imageBase, imageExt, pageCount)
  where
    -- Extract the preload data json.
    extractContent = take 1 . dropWhile (~/= ("<meta id=meta-preload-data>" :: String))
    Just json = Aeson.decode $ fromAttrib "content" $ head $ extractContent $ parseTags $ getResponseBody response
    obj = json ! "illust" ! AesonKey.fromString contentId

    -- Extract `artwork`.
    Aeson.String artist = obj ! "userName"
    Aeson.String rawDate = obj ! "createDate"
    dateRegex = "^[0-9]{2}([0-9]{2})-([0-9]{2})-([0-9]{2})" :: String
    (_, _, _, [year, month, day]) = unpack rawDate =~ dateRegex :: (String, String, String, [String])
    date = year <> month <> day
    Aeson.String title = obj ! "title"
    artwork = Artwork {artist = unpack artist, date = date, title = unpack title, contentId = contentId}

    -- Extract `pageCount`.
    Aeson.Number rawPageCount = obj ! "pageCount"
    Just pageCount = Scientific.toBoundedInteger rawPageCount

    -- Extract `imageBase` and `imageExt`.
    Aeson.String rawUrl = obj ! "urls" ! "original"
    urlRegex = "[0-9]+(\\.[a-z]+)$" :: String
    (imageBase, _, _, [imageExt]) = unpack rawUrl =~ urlRegex :: (String, String, String, [String])

(!) :: Aeson.Value -> Aeson.Key -> Aeson.Value
Aeson.Object obj ! field = map Map.! field
  where
    map = AesonKeyMap.toMap obj

crawlImage :: String -> Artwork -> String -> String -> Int -> IO ()
crawlImage contentId artwork imageBase imageExt pageCount = do
  let url = imageBase ++ show pageCount ++ imageExt
  response <- requestUrl url retry
  let imagePath = path ++ show artwork ++ "/" ++ contentId ++ "_p" ++ show pageCount ++ imageExt
  case response of
    Left _ -> putStrLn ("    Fail to crawl P" ++ show pageCount)
    Right response -> case getResponseStatusCode response of
      200 -> LBS.writeFile imagePath $ getResponseBody response
      _ -> putStrLn ("    Fail to crawl P" ++ show pageCount)
