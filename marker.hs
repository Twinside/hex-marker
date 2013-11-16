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
import System.Environment( getArgs )

import Jpg
import Tiff

import Debug.Trace
import Text.Printf

main :: IO ()
main = do
    args <- getArgs
    case args of
      (filename:outfile:_) -> do
          f <- B.readFile filename
          let tiff = getP "TiffFile" f :: Marker TiffInfo
              {-jpg = parseMark "" :: Marker JpgImage-}
          let out = renderByteDump f tiff
          L.writeFile outfile out

      _ -> putStrLn "not enough arguments"

