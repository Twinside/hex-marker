{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative( (<$>), (<*>) )
import Control.Monad( when, replicateM, forM )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Int(Int16)
import Data.Marker
import qualified Data.Text as T
import Data.Word

import Data.Marker
import Data.Marker.Jpg
import Data.Marker.Tiff
import Data.Marker.Gif
import System.Environment( getArgs )

import Debug.Trace
import Text.Printf

putHelp :: IO ()
putHelp = putStrLn $
    "marker <format> <infile> <outfile>\n" ++
    "where format can be:\n" ++
    " * gif\n" ++
    " * jpg\n" ++
    " * tiff\n"

main :: IO ()
main = do
    args <- getArgs
    case args of
      ("--help":_) -> putHelp
      ("tiff":filename:outfile:_) -> do
          f <- B.readFile filename
          let out = renderByteDump f $ markTiff f
          L.writeFile outfile out

      ("jpg":filename:outfile:_) -> do
          f <- B.readFile filename
          L.writeFile outfile $ renderByteDump f markJpeg

      ("gif":filename:outfile:_) -> do
          f <- B.readFile filename
          L.writeFile outfile $ renderByteDump f markGif

      _ -> putStrLn "not enough arguments"

