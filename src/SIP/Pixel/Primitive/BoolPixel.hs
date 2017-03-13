-----------------------------------------------------------------------------
-- |
-- Module      :  SIP.Pixel.Primitive.BoolPixel
-- Copyright   :  (c) Gilberto Vargas
-- License     :  BSD3
-- Maintainer  :  tachoguitar@gmail.com
-- Stability   :  development
-- Portability :  portable
--
--

module SIP.Pixel.Primitive.BoolPixel where

  newtype BoolPixel = BoolPixel Bool deriving (Eq, Show, Ord, Bounded, Read)
