module Myfile where

import System.IO(IOMode(..), openFile, hClose, hGetContents, hSetEncoding, utf8, hPutStr)
import System.Directory(doesFileExist)
import Mydata(tgPass)

isFile :: IO Bool
isFile = doesFileExist tgPass

fileR :: IO String 
fileR = do
  h <- openFile tgPass ReadMode
  hSetEncoding h utf8
  con <- hGetContents h 
  putStr (con++"\n")
  hClose h
  return con

fileW :: String -> IO ()
fileW str = do
  h <- openFile tgPass WriteMode
  hSetEncoding h utf8
  hPutStr h str 
  hClose h

