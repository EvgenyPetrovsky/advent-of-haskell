module Util.Geometry (
      segm
    , pointOnSegm
    , Ray
    , rayInt
) where


--pointOnLine :s: Num a => (a,a,a) -> (a,a) -> Bool
--pointOnLine lile_equation point = undefined

--rectangle :: Num a => (a,a) -> (a,a) -> ((a,a),(a,a))
--rectangle corner1@(x1,y1) corner2@(x2,y2) = ((min x1 xy, min y1 y2), (max x1 xy, max y1 y2))

data Point a = Point a a deriving Show
data Ray a = Ray a a deriving Show

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


{- produce a ray with start point and the point on the distance of 1 from starting point -}
rayInt :: Point Int -> Point Int -> Ray (Point Int)
rayInt start@(Point x0 y0) (Point x1 y1) =
  let (dx,dy) = (x1 - x0, y1 - y0)
      d = gcd (abs dx) (abs dy)
  in Ray start $ Point (x0 + (dx `div` d)) (y0 + (dy `div` d))

