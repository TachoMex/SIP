-----------------------------------------------------------------------------
-- |
-- Module      :  SIP.Image
-- Copyright   :  (c) Gilberto Vargas
-- License     :  BSD3
-- Maintainer  :  tachoguitar@gmail.com
-- Stability   :  development
-- Portability :  portable
--
--

module SIP.Image where
  import Data.Sequence as S
  data Image a  = Image {
    pixels :: Seq a,
    width :: Int,
    height :: Int
  } deriving (Show)

  -- Accessors

  at :: Image a -> Int -> Int -> a
  at (Image p w h) x y = S.index p idx
    where
      idx = x * h + y

  lookup :: Image a -> Int -> Int -> Maybe a
  lookup (Image p w h) x y = S.lookup idx p
    where
      idx = x * h + y

  replace :: Image a -> a -> Int -> Int -> Image a
  replace (Image p w h) pix x y = Image p' w h
    where
      idx = x * h + y
      p' = S.update idx pix p

  replace' :: Image a -> a -> (Int, Int) -> Image a
  replace' i c (x, y) = replace i c y x

  -- Initializers
  consColor :: a -> Int -> Int -> Image a
  consColor c h w = Image p w h
    where p = S.replicate (w * h) c

  instance Functor Image where
    fmap f (Image p w h) = Image p' w h
      where
        p' = fmap f p
