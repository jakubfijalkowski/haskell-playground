module Bitmap.BMP (saveAsBMP) where

import           Bitmap                       (Bitmap (Bitmap, bData, bWidth),
                                               Pixel (Pixel))

import           Control.Exception            (catch)
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits                    (complement)
import qualified Data.ByteString.Lazy         as BS
import           Data.ByteString.Lazy.Builder
import           Data.Monoid

saveAsBMP :: FilePath -> Bitmap -> IO (Either String ())
saveAsBMP p b = catch (fmap Right $ BS.writeFile p (encode $ buildBMP b)) (\e ->
  return $ Left $ "cannot open file " ++ (show (e :: IOError)))

data BMPImage = BMPImage {
  bmpHeader :: BMPHeader,
  bmpDib    :: DIBHeader,
  bmpData   :: BS.ByteString
}

data BMPHeader = BMPHeader {
  bmphMagic      :: Word16,
  bmphFileSize   :: Word32,
  bmphReserved1  :: Word32,
  bmphDataOffset :: Word32
}

-- BITMAPV5HEADER
data DIBHeader = DIBHeader {
  dibSize            :: Word32,
  dibWidth           :: Word32,
  dibHeight          :: Word32,
  dibColorPlanes     :: Word16,
  dibBitsPerPixel    :: Word16,
  dibCompression     :: Word32,
  dibImageSize       :: Word32,
  dibHRes            :: Word32,
  dibVRes            :: Word32,
  dibColorsInPalette :: Word32,
  dibImportantColors :: Word32,
  dibRedMask         :: Word32,
  dibGreenMask       :: Word32,
  dibBlueMask        :: Word32,
  dibAlphaMask       :: Word32,
  dibCSType          :: Word32,
  dibEndpoints       :: CIEXYZTRIPLE,
  dibGammaRed        :: Word32,
  dibGammaGreen      :: Word32,
  dibGammaBlue       :: Word32,
  dibIntent          :: Word32,
  dibProfileData     :: Word32,
  dibProfileSize     :: Word32,
  dibReserved        :: Word32
}

data CIEXYZ = CIEXYZ Word32 Word32 Word32
data CIEXYZTRIPLE = CIEXYZTRIPLE CIEXYZ CIEXYZ CIEXYZ

buildBMP :: Bitmap -> BMPImage
buildBMP b = BMPImage {
  bmpHeader = buildBMPHeader b,
  bmpDib    = buildDIBHeader b,
  bmpData   = toBGRData (bData b) (bWidth b)
}

buildDIBHeader :: Bitmap -> DIBHeader
buildDIBHeader (Bitmap w h _) = DIBHeader {
  dibSize            = 124,
  dibWidth           = fromIntegral w,
  dibHeight          = fromIntegral h,
  dibColorPlanes     = 1,
  dibBitsPerPixel    = 24,
  dibCompression     = 0,
  dibImageSize       = 0,
  dibHRes            = 96,
  dibVRes            = 96,
  dibColorsInPalette = 0,
  dibImportantColors = 0,
  dibRedMask         = 0,
  dibGreenMask       = 0,
  dibBlueMask        = 0,
  dibAlphaMask       = 0,
  dibCSType          = 0x73524742,
  dibEndpoints       = buildCIEXYZTRIPLE,
  dibGammaRed        = 0,
  dibGammaGreen      = 0,
  dibGammaBlue       = 0,
  dibIntent          = 2,
  dibProfileData     = 0,
  dibProfileSize     = 0,
  dibReserved        = 0
}

buildBMPHeader :: Bitmap -> BMPHeader
buildBMPHeader (Bitmap w h _) = BMPHeader {
  bmphMagic      = 0x424D,
  bmphFileSize   = 14 + 124 + fromIntegral (w * h * 3),
  bmphReserved1  = 0,
  bmphDataOffset = 14 + 124
}

buildCIEXYZ :: CIEXYZ
buildCIEXYZ = CIEXYZ 0 0 0

buildCIEXYZTRIPLE :: CIEXYZTRIPLE
buildCIEXYZTRIPLE = CIEXYZTRIPLE buildCIEXYZ buildCIEXYZ buildCIEXYZ

instance Binary BMPHeader where
  put h = do putWord16be $ bmphMagic h
             putWord32le $ bmphFileSize h
             putWord32le $ bmphReserved1 h
             putWord32le $ bmphDataOffset h
  get = do m <- getWord16be
           fs <- getWord32le
           r <- getWord32le
           dof <- getWord32le
           return BMPHeader {
             bmphMagic      = m,
             bmphFileSize   = fs,
             bmphReserved1  = r,
             bmphDataOffset = dof
           }

instance Binary DIBHeader where
  put d = do putWord32le $ dibSize d
             putWord32le $ dibWidth d
             putWord32le $ (complement $ dibHeight d) + 1
             putWord16le $ dibColorPlanes d
             putWord16le $ dibBitsPerPixel d
             putWord32le $ dibCompression d
             putWord32le $ dibImageSize d
             putWord32le $ dibHRes d
             putWord32le $ dibVRes d
             putWord32le $ dibColorsInPalette d
             putWord32le $ dibImportantColors d
             putWord32le $ dibRedMask d
             putWord32le $ dibGreenMask d
             putWord32le $ dibBlueMask d
             putWord32le $ dibAlphaMask d
             putWord32le $ dibCSType d
             put $ dibEndpoints d
             putWord32le $ dibGammaRed d
             putWord32le $ dibGammaGreen d
             putWord32le $ dibGammaBlue d
             putWord32le $ dibIntent d
             putWord32le $ dibProfileData d
             putWord32le $ dibProfileSize d
             putWord32le $ dibReserved d
  get = do s <- getWord32le
           w <- getWord32le
           h <- getWord32le
           cp <- getWord16le
           bpp <- getWord16le
           c <- getWord32le
           is <- getWord32le
           hr <- getWord32le
           vr <- getWord32le
           cip <- getWord32le
           ic <- getWord32le
           rm <- getWord32le
           gm <- getWord32le
           bm <- getWord32le
           am <- getWord32le
           cst <- getWord32le
           endp <- get
           gr <- getWord32le
           gg <- getWord32le
           gb <- getWord32le
           int <- getWord32le
           pd <- getWord32le
           ps <- getWord32le
           r <- getWord32le
           return DIBHeader {
             dibSize            = s,
             dibWidth           = w,
             dibHeight          = h,
             dibColorPlanes     = cp,
             dibBitsPerPixel    = bpp,
             dibCompression     = c,
             dibImageSize       = is,
             dibHRes            = hr,
             dibVRes            = vr,
             dibColorsInPalette = cip,
             dibImportantColors = ic,
             dibRedMask         = rm,
             dibGreenMask       = gm,
             dibBlueMask        = bm,
             dibAlphaMask       = am,
             dibCSType          = cst,
             dibEndpoints       = endp,
             dibGammaRed        = gr,
             dibGammaGreen      = gg,
             dibGammaBlue       = gb,
             dibIntent          = int,
             dibProfileData     = pd,
             dibProfileSize     = ps,
             dibReserved        = r
           }

instance Binary CIEXYZTRIPLE where
  put _ = do putWord32le 0
             putWord32le 0
             putWord32le 0
             putWord32le 0
             putWord32le 0
             putWord32le 0
             putWord32le 0
             putWord32le 0
             putWord32le 0
  get = return buildCIEXYZTRIPLE

instance Binary BMPImage where
  put i = do put $ bmpHeader i
             put $ bmpDib i
             putLazyByteString $ bmpData i
  get = do h <- get
           dib <- get
           d <- get
           return (BMPImage h dib d)


toBGRData :: [Pixel] -> Int -> BS.ByteString
toBGRData ps = toLazyByteString . toBGRData' ps

toBGRData' :: [Pixel] -> Int -> Builder
toBGRData' [] _ = mempty
toBGRData' ps w = let (row, rest) = splitAt w ps
                      rowSize = ((24 * w + 31) `div` 32) * 4
                  in mappend (toBGRRow row rowSize) (toBGRData' rest w)

toBGRRow :: [Pixel] -> Int -> Builder
toBGRRow [] l = lazyByteString $ BS.pack $ replicate l 0
toBGRRow (p:ps) l = mappend (toBGR p) (toBGRRow ps (l - 3))

toBGR :: Pixel -> Builder
toBGR (Pixel r g b) = mconcat $ map word8 [b, g, r]
