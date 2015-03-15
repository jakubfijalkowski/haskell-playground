module Main where

import System.Environment (getArgs, getProgName)
import Bitmap.BMP (saveAsBMP)
import Bitmap.PBM (readPGM)

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  if length args /= 2
  then putStrLn ("Usage: " ++ progName ++ " source.pbm destination.bmp")
  else convert (head args) (last args)

convert :: FilePath -> FilePath -> IO ()
convert src dst = do
  cnt <- readPGM src
  case cnt of
    Left err  -> putStrLn ("Cannot decode file - " ++ err)
    Right bmp -> do
      result <- saveAsBMP dst bmp
      case result of
        Left err -> putStrLn ("Cannot save file - " ++ err)
        Right () -> putStrLn "File successfully converted!"