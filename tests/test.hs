module Main where

import Crawler (crawlArtwork)
import System.Directory (getFileSize, listDirectory, removePathForcibly)
import System.Exit (exitFailure, exitSuccess)

path :: String
path = "./[コーラ] [200611] 雨の町 (82255171)"

main :: IO ()
main = do
  crawlArtwork "82255171"
  files <- listDirectory path
  p0Size <- getFileSize (path ++ "/82255171_p0.jpg")
  p1Size <- getFileSize (path ++ "/82255171_p1.jpg")
  p2Size <- getFileSize (path ++ "/82255171_p2.jpg")
  case (length files, p0Size, p1Size, p2Size) of
    (3, 2282431, 2108130, 1400864) -> removePathForcibly path >> exitSuccess
    _ -> exitFailure
