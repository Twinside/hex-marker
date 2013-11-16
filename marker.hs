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
import System.Environment( getArgs )

import Debug.Trace
import Text.Printf

main :: IO ()
main = do
    args <- getArgs
    case args of
      ("tiff":filename:outfile:_) -> do
          f <- B.readFile filename
          let out = renderByteDump f $ markTiff f
          L.writeFile outfile out

      ("jpg":filename:outfile:_) -> do
          f <- B.readFile filename
          L.writeFile outfile $ renderByteDump f markJpeg


      _ -> putStrLn "not enough arguments"

