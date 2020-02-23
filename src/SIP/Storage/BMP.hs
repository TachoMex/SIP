{-# LANGUAGE OverloadedStrings #-}
module SIP.Storage.BMP where
  import Data.Binary
  import System.IO
  import GHC.IO.Handle
  import qualified Data.ByteString.Lazy as B
  import qualified Data.Sequence as S

  import Data.Int
  import Data.Foldable (toList)
  import Data.List.Split (chunksOf)
  import Data.List (intersperse,intercalate)
  import Control.Monad (forM_, join, replicateM, replicateM_)

  import SIP.Image hiding (width, height)
  import SIP.Pixel.Primitive.U1V
  import SIP.Pixel.Primitive.U3V
  import SIP.Pixel.Primitive.U4V
  import SIP.Pixel.Primitive.BoolPixel
  import Hexdump

  data BMPHeader = BMPHeader {
    signature :: B.ByteString, -- 'BM'
    fileSize :: Int32, -- 4 bytes
    reserved1 :: Int16, -- 2 bytes, always zero
    reserved2 :: Int16, -- 2 bytes, always zero
    offset :: Int32, -- 4 bytes
    headerSize :: Int32, -- 4 bytes, always 40
    width :: Int32, -- 4 bytes
    height :: Int32, -- 4 bytes
    planesNumber :: Int16, -- 2 bytes, set to 1.
    pixelSize :: Int16, -- 2 bytes, always set to 1, 4, 8 or 24.
    compression :: Int32, -- 2 bytes, 0 none, 1 RLE 8, 2 RLE 4.
    imageSize :: Int32, -- 2 bytes, if file has no compresion 0 is ok.
    horizontalResolution :: Int32, -- 4 bytes
    verticalResolution :: Int32, -- 4 bytes
    numberOfColors :: Int32, -- 4 bytes
    numberOfImportantColors :: Int32, -- 4 bytes
    palette :: B.ByteString
  } deriving (Show)

  defaultBMPHeader = BMPHeader {
    signature = "BM",
    fileSize = 0,
    reserved1 = 0,
    reserved2 = 0,
    offset = 0,
    headerSize = 40,
    width = 0,
    height = 0,
    planesNumber = 1,
    pixelSize = 24,
    compression = 0,
    imageSize = 0,
    horizontalResolution = 0,
    verticalResolution = 0,
    numberOfColors = 0,
    numberOfImportantColors = 0,
    palette = ""
  }


  -- TODO: Change U3V to any  primary color.
  store :: Image U3V -> String -> IO ()
  store (Image p w h) filename = do
    let padding = (4 - ((3 * w) `mod` 4)) `mod` 4
    let imgSize = 54 + (w + padding) * h * 3
    let header = defaultBMPHeader {
          fileSize = fromIntegral imgSize,
          offset = 0x36,
          width = fromIntegral w,
          height = fromIntegral h,
          imageSize = fromIntegral $ w * h * 3
        }
    let p' = pixelBytes (toList p) w
    handle <- openFile filename WriteMode
    writeHeader handle header
    B.hPut handle p'
    hClose handle

  writeHeader :: Handle ->  BMPHeader -> IO ()
  writeHeader handle header = do
    B.hPut handle . signature $ header
    B.hPut handle . B.reverse . encode . fileSize $ header
    B.hPut handle . B.reverse . encode . reserved1 $ header
    B.hPut handle . B.reverse . encode . reserved2 $ header
    B.hPut handle . B.reverse . encode . offset $ header
    B.hPut handle . B.reverse . encode . headerSize $ header
    B.hPut handle . B.reverse . encode . height $ header
    B.hPut handle . B.reverse . encode . width $ header
    B.hPut handle . B.reverse . encode . planesNumber $ header
    B.hPut handle . B.reverse . encode . pixelSize $ header
    B.hPut handle . B.reverse . encode . compression $ header
    B.hPut handle . B.reverse . encode . imageSize $ header
    B.hPut handle . B.reverse . encode . horizontalResolution $ header
    B.hPut handle . B.reverse . encode . verticalResolution $ header
    B.hPut handle . B.reverse . encode . numberOfColors $ header
    B.hPut handle . B.reverse . encode . numberOfImportantColors $ header
    B.hPut handle . palette $ header
    return ()

  readHeader :: Handle -> IO BMPHeader
  readHeader handle = do
    sgtr <- B.hGet handle 2
    filesz <- B.hGet handle 4
    res1 <- B.hGet handle 2
    res2 <- B.hGet handle 2
    offs <- B.hGet handle 4
    hsize <- B.hGet handle 4
    h <- B.hGet handle 4
    w <- B.hGet handle 4
    pn <- B.hGet handle 2
    pixSize <- B.hGet handle 2
    cmprss <- B.hGet handle 4
    isize <- B.hGet handle 4
    hres <- B.hGet handle 4
    vres <- B.hGet handle 4
    noc <- B.hGet handle 4
    noic <- B.hGet handle 4
    pltt <- B.hGet handle 0
    let header = BMPHeader {
      signature = sgtr,
      fileSize = decode $ B.reverse filesz,
      reserved1 = decode $ B.reverse res1,
      reserved2 = decode $ B.reverse res2,
      offset = decode $ B.reverse offs,
      headerSize = decode $ B.reverse hsize,
      width = decode $ B.reverse w,
      height = decode $ B.reverse h,
      planesNumber = decode $ B.reverse pn,
      pixelSize = decode $ B.reverse pixSize,
      compression = decode $ B.reverse cmprss,
      imageSize = decode $ B.reverse isize,
      horizontalResolution = decode $ B.reverse hres,
      verticalResolution = decode $ B.reverse vres,
      numberOfColors = decode $ B.reverse noc,
      numberOfImportantColors = decode $ B.reverse noic,
      palette = pltt
    }
    hSetPosn . HandlePosn handle . fromIntegral $ fileSize header - imageSize header

    return header

  pixelBytes :: [U3V] -> Int -> B.ByteString
  pixelBytes pixels w = B.drop 8 . encode . intercalate padding $ fmap rowToBytes rows
    where
      rows = chunksOf w pixels :: [[U3V]]
      rowToBytes :: [U3V] -> [Int8]
      rowToBytes r = join $ fmap writePixel r
      writePixel (U3V r g b) = [b', g', r']
        where
          b' = toWord b
          g' = toWord g
          r' = toWord r
      padding = replicate paddingSize 0
      toWord v = fromIntegral . round $ v * 255 :: Int8
      paddingSize = (4 - ((w * 3) `mod` 4)) `mod` 4

  -- TODO: Change U3V to any primary color.
  load :: String -> IO (Image U3V)
  load filename = do
    handle <- openFile filename ReadMode
    header <- readHeader handle
    print header
    pixels <- readPixels handle (width header) (height header) (pixelSize header) (palette header)
    return $ Image (S.fromList pixels) (fromIntegral $ width header) (fromIntegral $ height header)

  readPixels :: Handle -> Int32 -> Int32 -> Int16 -> B.ByteString -> IO [U3V]
  readPixels handle w h 24 "" = join <$> replicateM (fromIntegral h) (readRow handle)
    where
      paddingSize = (4 - ((w * 3) `mod` 4)) `mod` 4
      readRow handle = do
        row <- replicateM (fromIntegral w) (readPixel handle)
        B.hGet handle $ fromIntegral paddingSize
        return row
      readPixel handle = do
        a <- B.hGet handle 1
        b <- B.hGet handle 1
        c <- B.hGet handle 1
        return $ U3V (toF $ decode c) (toF $ decode b) (toF $ decode a)
      toF :: Int8 -> Double
      toF n = fromIntegral n / 255
