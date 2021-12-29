{-# LANGUAGE OverloadedStrings #-}

module Crawler (main) where

import Control.Exception (try)
import qualified Data.Aeson as Aeson(Key, Value (Number, Object, String), decode)
import qualified Data.Aeson.Key as Key (fromString)
import qualified Data.Aeson.KeyMap as Aeson (toMap)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS (writeFile)
import qualified Data.ByteString.Lazy.UTF8 as LBU
import qualified Data.Map as Map ((!))
import qualified Data.Scientific as Scientific (toBoundedInteger)
import Data.Text (Text, pack, unpack)
import Network.HTTP.Simple (HttpException, Response, getResponseBody, httpLBS, parseRequest, setRequestHeaders)
import System.Directory (createDirectory)
import System.Environment (getArgs)
import Text.HTML.TagSoup (fromAttrib, parseTags, (~/=))
import Text.Regex.TDFA ((=~))

retry :: Int
retry = 1

path = "/mnt/c/Users/33160/Desktop/"

userAgent =
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) \
  \Chrome/96.0.4664.110 Safari/537.36 Edg/96.0.1054.62"

data Artwork = Artwork {artist :: String, date :: String, title :: String, contentId :: String}

instance Show Artwork where
  show (Artwork artist date title contentId) = "[" ++ artist ++ "] [" ++ date ++ "] " ++ title ++ " (" ++ contentId ++ ")"

main :: IO ()
main = getArgs >>= mapM_ crawlArtwork

crawlArtwork :: String -> IO ()
crawlArtwork contentId = do
  putStrLn ("Crawl " ++ contentId)
  response <- requestUrl ("https://www.pixiv.net/artworks/" ++ contentId) retry
  case response of
    Left _ -> print ("    Fail to crawl " ++ contentId)
    Right response -> do
      let (artwork, imgBase, imgExt, pageCount) = extractArtwork contentId response
      createDirectory $ path ++ show artwork
      mapM_ (crawlImage contentId artwork imgBase imgExt) [0 .. pageCount - 1]
      print "done"

requestUrl :: String -> Int -> IO (Either HttpException (Response ByteString))
requestUrl url retry = do
  request <- parseRequest url
  let headers = [("User-Agent", userAgent), ("Referer", "https://www.pixiv.net/")]
  response <- try $ httpLBS $ setRequestHeaders headers request
  case (retry, response) of
    (retry, Left _) | retry > 0 -> requestUrl url (retry - 1)
    (_, response) -> return response

extractArtwork :: String -> Response ByteString -> (Artwork, String, String, Int)
extractArtwork contentId response = (artwork, unpack imgBase, unpack imgExt, pageCount)
  where
    extractContent = take 1 . dropWhile (~/= ("<meta id=meta-preload-data>"::String))
    content = fromAttrib "content" $ head $ extractContent $ parseTags $ getResponseBody response
    Just json = Aeson.decode content
    obj = json ! "illust" ! Key.fromString contentId
    Aeson.String artist = obj ! "userName"
    Aeson.String rawDate = obj ! "createDate"
    dateRegex = "^[0-9]{2}([0-9]{2})-([0-9]{2})-([0-9]{2})" :: Text
    (_, _, _, [year, month, day]) = rawDate =~ dateRegex:: (Text,Text,Text,[Text])
    date = year <> month <> day
    Aeson.String title = obj ! "title"
    artwork = Artwork {artist = unpack artist, date = unpack date, title = unpack title, contentId = contentId}
    Aeson.Number rawPageCount = obj ! "pageCount"
    Just pageCount = Scientific.toBoundedInteger rawPageCount
    Aeson.String rawUrl = obj ! "urls" ! "original"
    urlRegex = "[0-9]+(\\.[a-z]+)$" ::String
    (imgBase, _, _, [imgExt]) = rawUrl =~ urlRegex :: (Text,Text,Text,[Text])

(!) :: Aeson.Value -> Aeson.Key -> Aeson.Value
Aeson.Object obj ! field = do
  let map = Aeson.toMap obj
  map Map.! field

crawlImage :: String -> Artwork -> String -> String -> Int -> IO ()
crawlImage contentId artwork imgBase imgExt pageCount = do
    let url = imgBase ++ show pageCount ++ imgExt
    response <- requestUrl url retry
    let path = path ++ show artwork ++ "/" ++ contentId ++ "_p" ++ show pageCount ++ imgExt
    case response of
        Left _ -> putStrLn ("    Fail to crawl P" ++ show pageCount)
        Right response -> LBS.writeFile path $getResponseBody response
