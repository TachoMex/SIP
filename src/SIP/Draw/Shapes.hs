module SIP.Draw.Shapes(
  line,
  circle,
  circle',
  rectangle,
  rectangle',
  ellipse,
  ellipse',
  filler,
  curve
)where
  import SIP.Image
  import Debug.Trace

  line beg end =  apply points
    where
      points = bressemhamLine beg end


  bressemhamLine (xi, yi) (xf, yf)
    | dx == 0 = [(xi, y) | y <- [min_y..max_y]]
    | dy == 0 = [(x, yi) | x <- [min_x..max_x]]
    | dx == dy = [(xi + i, yi +  i)| i <- [0..dx]]
    | dy < dx =  if xi > xf
                  then bressemhamX (xf, yf) (dx, dy) (xi, yi) (2*dy - dx)
                  else bressemhamX (xi, yi) (dx, dy) (xf, yf) (2*dy - dx)
    | otherwise = if yi > yf
                  then bressemhamY (xf, yf) (dx, dy) (xi, yi) (2*dx - dy)
                  else bressemhamY (xi, yi) (dx, dy) (xf, yf) (2*dx - dy)
      where
        dx = abs(xf - xi)
        dy = abs(yf - yi)
        min_y = min yi yf
        max_y = max yi yf
        min_x = min xi xf
        max_x = max xi xf

  bressemhamX (xf, yf) (dx, dy) (x, y) p = bressemhamX' (x, y) p []
   where
     bressemhamX' (x, y) p ac
      | x == xf || y == yf = addCurrent
      | p > 0 = bressemhamX' (x + 1, y + inc) (p + 2*dy - 2*dx) addCurrent
      | otherwise = bressemhamX' (x + 1, y) (p + 2 * dy) addCurrent
      where
        addCurrent = (x, y):ac
        inc = if y < yf then 1 else (-1)

  bressemhamY (xf, yf) (dx, dy) (xi, yi) p = bressemhamY' (xi, yi) p []
   where
     bressemhamY' (x, y) p ac
       | x == xf || y == yf = addCurrent
       | p > 0 = bressemhamY' (x + inc, y + 1) (p + 2*dx - 2 * dy) addCurrent
       | otherwise = bressemhamY' (x, y + 1) (p + 2 * dx) addCurrent
       where
         addCurrent = (x, y):ac
         inc = if xi < xf then 1 else (-1)


  apply p c i = foldl apply' i p
    where
      apply' i = replace' i c

  circle = id
  circle' = id
  rectangle = id
  rectangle' (x, y) (w, h) = apply [(x + i, y + j) | i <- [0..w], j <- [0..h]] 
  ellipse = id
  ellipse' = id
  filler = id
  curve = id
