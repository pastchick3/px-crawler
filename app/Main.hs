module Main where

import Crawler (crawlArtwork)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= mapM_ crawlArtwork
