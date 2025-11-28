module Util.Geometry (
      segm
    , pointOnSegm
) where


--pointOnLine :: Num a => (a,a,a) -> (a,a) -> Bool
--pointOnLine lile_equation point = undefined

--rectangle :: Num a => (a,a) -> (a,a) -> ((a,a),(a,a))
--rectangle corner1@(x1,y1) corner2@(x2,y2) = ((min x1 xy, min y1 y2), (max x1 xy, max y1 y2))

segm :: (a,a) -> (a,a) -> ((a,a), (a,a))
segm p1 p2 = (p1,p2)

pointOnSegm :: (Ord a, Fractional a) =>(a,a) -> ((a,a), (a,a)) -> Bool
pointOnSegm (x,y) ((x1, y1), (x2, y2))
  | x < min x1 x2 || x > max x1 x2 = False
  | y < min y1 y2 || y > max y1 y2 = False
  | x == x1 && y == y1 || x == x2 && y == y2 = True
  | x1 == x2 && x == x2 && y <= max y1 y2 && y >= min y1 y2 = True
  | y1 == y2 && y == y2 && y <= max x1 x2 && x >= min x1 y2 = True
  | (x - x1) / (x2 - x1) == (y - y1) / (y2 - y1) = True
  | otherwise = False
