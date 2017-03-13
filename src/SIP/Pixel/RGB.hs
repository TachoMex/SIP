-----------------------------------------------------------------------------
-- |
-- Module      :  SIP.Pixel.RGB
-- Copyright   :  (c) Gilberto Vargas
-- License     :  BSD3
-- Maintainer  :  tachoguitar@gmail.com
-- Stability   :  development
-- Portability :  portable
--
--

module SIP.Pixel.RGB where
  import SIP.Pixel
  import SIP.Pixel.Primitive.U1V
  import SIP.Pixel.Primitive.U3V
  import SIP.Pixel.Primitive.U4V
  import SIP.Pixel.Primitive.BoolPixel
  import SIP.Pixel.Colors

  data RGB = RGB {
    r :: Double,
    g :: Double,
    b :: Double
  } deriving (Show, Read, Eq)

  instance Color RGB where
    red     = RGB 1 0 0
    green   = RGB 0 1 0
    blue    = RGB 0 0 1
    cyan    = RGB 0 1 1
    magenta = RGB 1 0 1
    yellow  = RGB 1 1 0
    black   = RGB 0 0 0
    white   = RGB 1 1 1
    gray    = RGB 0.5 0.5 0.5
    orange  = RGB 1 0.5 0
    pink    = RGB 1 0.5 1
    brown   = RGB 0.2 0.1 0

  instance Pixel RGB where
    to3V (RGB a b c) = U3V a b c
    to4V = to4V . to3V
    toGray = toGray . to3V
    toBoolPixel = toBoolPixel . to3V
    from3V (U3V a b c) = RGB a b c
    from4V (U4V a b c _) =  RGB a b c
    fromGray (U1V v) = RGB v v v
    fromBool (BoolPixel False) = RGB 0 0 0
    fromBool (BoolPixel True) = RGB 1 1 1
