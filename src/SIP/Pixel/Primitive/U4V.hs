-----------------------------------------------------------------------------
-- |
-- Module      :  SIP.Pixel.Primitive.U4V
-- Copyright   :  (c) Gilberto Vargas
-- License     :  BSD3
-- Maintainer  :  tachoguitar@gmail.com
-- Stability   :  development
-- Portability :  portable
--
--

module SIP.Pixel.Primitive.U4V where

  data U4V = U4V Double Double Double Double deriving (Eq, Show, Ord, Read)
