module Y19.D03
    ( parse
    , solve1
    , solve2
    ) where

--import AOCutil ((|>))
import Data.Set (Set, union, intersection)
import qualified Data.Set as S (fromList, empty, toList)
type Problem = [Path]
type Answer = Int

data Move = Up Int | Dn Int | Lt Int | Rt Int deriving Show

type Path = [Move]

path :: Path -> [((Int, Int), Int)]
path = foldl addMove []


addMove :: [((Int, Int), Int)] -> Move -> [((Int, Int), Int)]
addMove p m
    | null p    = addMove' ((0,0),0) m
    | otherwise = addMove' (head p) m ++ p
    where
        addMove' :: ((Int,Int),Int) -> Move -> [((Int,Int),Int)]
        addMove' ((row,col),len) (Up n) = [((row+dn,col),len+dn) | dn <- reverse [1..n]]
        addMove' ((row,col),len) (Dn n) = [((row-dn,col),len+dn) | dn <- reverse [1..n]]
        addMove' ((row,col),len) (Lt n) = [((row,col-dn),len+dn) | dn <- reverse [1..n]]
        addMove' ((row,col),len) (Rt n) = [((row,col+dn),len+dn) | dn <- reverse [1..n]]

parseline :: String -> Path
parseline =
    map parseMove . words . substitute ',' ' '
    where
        parseMove :: String -> Move
        parseMove ('U':n) = Up $ read n
        parseMove ('D':n) = Dn $ read n
        parseMove ('L':n) = Lt $ read n
        parseMove ('R':n) = Rt $ read n
        parseMove unknown = error $ "can't parse move " ++ unknown
        substitute :: Eq a => a -> a -> [a] -> [a]
        substitute _    _  [] = []
        substitute from to (x:xs)
            | from == x = to: substitute from to xs
            | otherwise = x : substitute from to xs

parse :: String -> Problem
parse = map parseline . lines


solve1 :: Problem -> Answer
solve1 input =
    let projections = map (S.fromList . map fst . path) input
        (intersections, _) = foldl inter_union (S.empty, S.empty) projections
        distances = map (\(a,b) -> abs a + abs b) . S.toList $ intersections
    in minimum distances
    where
        inter_union :: (Set (Int, Int) , Set (Int, Int)) -> Set (Int, Int) -> (Set (Int, Int) , Set (Int, Int))
        inter_union (is, us) s = (is `union` (us `intersection` s), us `union` s)

solve2 :: Problem -> Answer
solve2 input =
    let ts = map path input
        (t1, t2) =
            case ts of
                x1:x2:_ -> (x1, x2)
                _       -> error "Input data contains less than 2 paths"
        projections  = map (S.fromList . map fst) [t1,t2]
        (intersections, _) = foldl inter_union (S.empty, S.empty) projections
        t1xl = [l | i <- S.toList intersections, l <- [l | (rc, l) <- t1, rc == i]]
        t2xl = [l | i <- S.toList intersections, l <- [l | (rc, l) <- t2, rc == i]]
     in minimum $ zipWith (+) t1xl t2xl
    where
        inter_union :: (Set (Int, Int) , Set (Int, Int)) -> Set (Int, Int) -> (Set (Int, Int) , Set (Int, Int))
        inter_union (is, us) s = (is `union` (us `intersection` s), us `union` s)
