module Y25.D07
    ( parse
    , solve1
    , solve2
    ) where
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map

type Position = Int
type Beam = Position
type Timeline = Int
type Splitter = Position
type Problem = (Beam, [[Splitter]])
type Answer = Int

data QBeam =
      QBeam Position Timeline
    | QFork QBeam QBeam

{-
Returns
    Start position of Beam (first line)
    List of Lists of positions of Splitters
        outer list preservs lines of file (top down)
        inner list contains splitters in the line
            where position is offset from the beginning of the line
-}
parse :: String -> Problem
parse text_input =
    (
        head . head $ list_of_pos 'S'
      , list_of_pos '^'
    )
    where
        list_of_pos :: Char -> [[Int]]
        list_of_pos c = [[idx | (chr, idx) <- zip r [0 ..], chr == c ] | r <- lines text_input]


split :: Int -> [Int]
split x = [x-1, x+1]

solve1 :: Problem -> Answer
solve1 (start, splitters_array) =
    split_cnt
    where
        (split_cnt, _) = foldl f (0,[start]) splitters_array
        pass_beam :: Beam -> [Splitter] -> [Beam]
        pass_beam b ss
            | b `elem` ss = split b
            | otherwise   = [b]
        f :: (Int, [Beam]) -> [Splitter] -> (Int, [Beam])
        f (cnt, bms) spls =
            let new_bms = map (`pass_beam` spls) bms
                new_cnt = cnt + sum (map (\b -> length b - 1) new_bms)
            in (new_cnt, nub $ concat new_bms)


{-
Quantum split
    split positions, inherit timenlines
-}
qsplit :: QBeam -> QBeam
qsplit (QBeam p t) = QFork (QBeam (p-1) t) (QBeam (p+1) t)
qsplit (QFork {}) = error "only QBeam can be split"


{-
Quantum merge
    increases counts of timelines
    while combining two beams with same position
-}
qmerge :: [QBeam] -> [QBeam]
qmerge [] = []
qmerge qbms =
    map (uncurry QBeam) . Map.toList $ foldl mrg Map.empty qbms
    where
        mrg :: Map Int Int -> QBeam -> Map Int Int
        mrg m (QBeam pos tml) = Map.insertWith (+) pos tml m
        mrg m (QFork (QBeam pos1 tml1) (QBeam pos2 tml2)) =
            Map.insertWith (+) pos1 tml1 . Map.insertWith (+) pos2 tml2 $ m
        mrg _ (QFork {}) = error "only QFork of QBeam can be split"


{-
Quantum pass
Step 1
    If splitter on the way - perform quantum split
    If no splitter, let it propagate
Step 2
    Merge all splitted beams and sum there timelines
-}
qpass :: QBeam -> [Splitter] -> QBeam
qpass qb@(QBeam pos _) ss
    | pos `elem` ss = qsplit qb
    | otherwise   = qb
qpass (QFork {}) _ = error "only beam can be split"


solve2 :: Problem -> Answer
solve2 (start, splitters) =
    let qinit  = QBeam start 1
        qfinal :: [QBeam]
        qfinal = foldl (\bms ss -> qmerge . map (`qpass` ss) $ bms) [qinit] splitters
    in sum . map get_timeline $ qfinal
    where
        get_timeline :: QBeam -> Int
        get_timeline (QBeam _ tml) = tml
        get_timeline (QFork {}) = 0
