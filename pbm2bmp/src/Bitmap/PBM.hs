module Bitmap.PBM (readPGM, parsePGM) where

import           Bitmap
import           Bitmap.Internal.Parsing
import           Control.Exception          (catch)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Word                  (Word8)

readPGM :: FilePath -> IO (Either String Bitmap)
readPGM p = do
  cnt <- catch (fmap Right $ BS.readFile p) (\e ->
    return $ Left (show (e :: IOError)))
  case cnt of
    Left err   -> return $ Left ("cannot open file: " ++ err)
    Right cnt' -> return $ parsePGM cnt'

parsePGM :: BS.ByteString -> Either String Bitmap
parsePGM str =
  case runParse parser (ParseState str 0) of
    Left err     -> Left err
    Right (b, _) -> Right b
  where parser =
          readString 2 ==> deduceFormat ==> \(comp, conv) -> skipSpaces ==>&
          readNumber ==> \width -> skipSpaces ==>&
          readNumber ==> \height -> skipSpaces ==>&
          readNumber ==> \maxGray -> skipSpaces ==>&
          readNumbersWith (width * height * comp) (fromIntegral . scaleValue maxGray) ==>
          \pixels -> identity (Bitmap width height (conv pixels))

parseP2 :: [Word8] -> [Pixel]
parseP2 = map grayscalePixel

parseP3 :: [Word8] -> [Pixel]
parseP3 [] = []
parseP3 (r:g:b:cs) = (Pixel r g b) : parseP3 cs

deduceFormat :: String -> Parse (Int, [Word8] -> [Pixel])
deduceFormat fmt
  | fmt == "P2" = identity (1, parseP2)
  | fmt == "P3" = identity (3, parseP3)
  | otherwise   = bail "invalid magic"

readNumbersWith :: Int -> (Int -> a) -> Parse [a]
readNumbersWith 0 _ = identity []
readNumbersWith n f =
  readNumber ==> \val -> skipSpaces ==>&
  fmap (f val:) (readNumbersWith (n - 1) f)

scaleValue :: Int -> Int -> Int
scaleValue m' v' = c
  where v = fromIntegral v' :: Double
        m = fromIntegral m'
        c' = v / m * 255
        c = truncate c'

grayscalePixel :: Word8 -> Pixel
grayscalePixel c = Pixel c c c
