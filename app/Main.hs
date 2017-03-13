module Main where
  import SIP.Pixel.RGB
  import SIP.Pixel.Primitive.U3V
  import SIP.Pixel
  import SIP.Draw.Shapes
  import SIP.Image
  import SIP.Storage.BMP
  import SIP.Pixel.Colors
  import Hexdump

  import Graphics.UI.GLUT
  import Data.Binary
  import Data.Word
  import Foreign
  import qualified Graphics.Rendering.OpenGL as GL
  import Data.ByteString.Unsafe
  import Data.Foldable (toList)
  import Control.Monad (forM_, join)
  import qualified Data.ByteString.Lazy as B
  import qualified Data.Sequence as S

  glRender display i@(Image _ w h) = do
    (_progName, _args) <- getArgsAndInitialize
    initialWindowSize $= Size (fromIntegral h) (fromIntegral w)
    window <- createWindow "Sample window"
    displayCallback $= display i
    mainLoop

  toBytes :: S.Seq U3V -> B.ByteString
  toBytes buf = B.drop 8 . encode $ bytes
    where
      bytes :: [Int8]
      bytes = join . toList $ fmap writePixel pixels
      pixels = toList buf
      writePixel (U3V r g b) = [r', g', b']
        where
          b' = toWord b
          g' = toWord g
          r' = toWord r
      toWord v = fromIntegral . round $ v * 255 :: Int8

  display (Image p w h) = do
    let sze = Size (fromIntegral h) (fromIntegral w)
    buffer <- unsafeUseAsCString (B.toStrict $ toBytes p) return
    drawPixels sze (GL.PixelData GL.RGB GL.UnsignedByte buffer)
    GL.flush

  negative (U3V r g b) = U3V (1-r) (1-g) (1-b)
  setRed (U3V _ g b) = U3V 1 g b

  main :: IO ()
  main = do
    -- i <- load "catedral.bmp"
    -- let i2 = setRed <$> foldl (\ x i -> replace x (U3V 1 0 0) i i) i [0..600]
    -- store i2 "catedral.prueba.bmp"
    let i = consColor black 640 480
    -- let i2 =  line (630,  10) ( 10,  10) blue .
    --           line (630, 470) (630,  10) green .
    --           line ( 10, 470) (630, 470) green .
    --           line ( 10,  10) ( 10, 470) white .
    --           line (630, 240) ( 10, 240) green .
    --           line (320, 470) (320,  10) green .
    --           line (320, 470) ( 10, 240) green .
    --           line ( 10, 240) (320,  10) green .
    --           line (320,  10) ( 10, 240) green $ i
    let i2 = rectangle' (10, 10) (20, 20) red .
             rectangle' (320, 240) (30, 50) blue $ i 
    -- let i2 = line (10, 250) (350, 10) red .
    --          line (10, 10) (690, 10) green $ i
    glRender display i2
