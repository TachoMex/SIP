-----------------------------------------------------------------------------
-- |
-- Module      :  SIP.Pixel
-- Copyright   :  (c) Gilberto Vargas
-- License     :  BSD3
-- Maintainer  :  tachoguitar@gmail.com
-- Stability   :  development
-- Portability :  portable
--
--

module SIP.Pixel where
  import SIP.Pixel.Primitive.U1V
  import SIP.Pixel.Primitive.U3V
  import SIP.Pixel.Primitive.U4V
  import SIP.Pixel.Primitive.BoolPixel

  class Pixel a where
    to3V :: a -> U3V
    to4V :: a -> U4V
    toGray :: a -> U1V
    toBoolPixel :: a -> BoolPixel
    from3V :: U3V -> a
    from4V :: U4V -> a
    fromGray :: U1V -> a
    fromBool :: BoolPixel -> a

  instance Pixel U3V where
    to3V = id
    to4V (U3V a b c) = U4V a b c 1
    toGray (U3V a b c) = U1V $ (a + b + c) / 3.0
    toBoolPixel = toBoolPixel . toGray
    from3V = id
    from4V (U4V a b c _) =  U3V a b c
    fromGray (U1V v) = U3V v v v
    fromBool (BoolPixel False) = U3V 0 0 0
    fromBool (BoolPixel True) = U3V 1 1 1

  instance Pixel U4V where
    to3V = from4V
    to4V = id
    toGray = toGray . to3V
    toBoolPixel = toBoolPixel . toGray
    from3V = to4V
    from4V = id
    fromGray (U1V v) = U4V v v v 1
    fromBool (BoolPixel False) = U4V 0 0 0 0
    fromBool (BoolPixel True) = U4V 1 1 1 1

  instance Pixel U1V where
    to3V = fromGray
    to4V = fromGray
    toGray = id
    toBoolPixel (U1V v) = BoolPixel $ v < 0.5
    from3V = toGray
    from4V = toGray
    fromGray = id
    fromBool (BoolPixel False) = U1V 0
    fromBool (BoolPixel True) = U1V 1

  instance Pixel BoolPixel where
    to3V = fromBool
    to4V = fromBool
    toGray = fromBool
    toBoolPixel = id
    from3V = toBoolPixel
    from4V = toBoolPixel
    fromGray = toBoolPixel
    fromBool = id

