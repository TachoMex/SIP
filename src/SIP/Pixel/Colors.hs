-----------------------------------------------------------------------------
-- |
-- Module      :  SIP.Pixel.Colors
-- Copyright   :  (c) Gilberto Vargas
-- License     :  BSD3
-- Maintainer  :  tachoguitar@gmail.com
-- Stability   :  development
-- Portability :  portable
--
--

module SIP.Pixel.Colors where
  import SIP.Pixel.Primitive.U3V
  class Color a where
    red     :: a
    green   :: a
    blue    :: a
    cyan    :: a
    magenta :: a
    yellow  :: a
    black   :: a
    white   :: a
    gray    :: a
    orange  :: a
    pink    :: a
    brown   :: a

  instance Color U3V where
    red     = U3V 1 0 0
    green   = U3V 0 1 0
    blue    = U3V 0 0 1
    cyan    = U3V 0 1 1
    magenta = U3V 1 0 1
    yellow  = U3V 1 1 0
    black   = U3V 0 0 0
    white   = U3V 1 1 1
    gray    = U3V 0.5 0.5 0.5
    orange  = U3V 1 0.5 0
    pink    = U3V 1 0.5 1
    brown   = U3V 0.2 0.1 0
