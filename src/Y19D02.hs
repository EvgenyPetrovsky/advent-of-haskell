module Y19D02
    ( parse
    , solve1
    , solve2
    ) where

type Problem = [Int]
type Answer = Int

type Program = [Int]

run :: Program -> Program
run program =
    go program 0
    where
        go :: [Int] -> Int -> [Int]
        go memory address =
            let opcode = memory !! (address + 0)
                stop = opcode == 99
            in if stop then memory else go (apply opcode) (fwd address)
            where

                fwd = (+) 4
                apply :: Int -> [Int]
                apply opcode
                  | opcode == 1 = replaceAt resadr (opval1 + opval2) memory
                  | opcode == 2 = replaceAt resadr (opval1 * opval2) memory
                  | opcode == 99 = memory
                  | otherwise = error $ "Unknown opcode = " ++ show opcode
                  where
                    opval1 = memory !! (memory !! (address + 1))
                    opval2 = memory !! (memory !! (address + 2))
                    resadr = memory !! (address + 3)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt n new_x xs = take n xs ++ [new_x] ++ drop (n+1) xs

parse :: String -> Problem
parse = map read . words . substitute ',' ' '
    where
        substitute :: Eq a => a -> a -> [a] -> [a]
        substitute _    _  [] = []
        substitute from to (x:xs)
          | from == x = to: substitute from to xs
          | otherwise = x : substitute from to xs


solve1 :: Problem -> Answer
solve1 = head . run . replaceAt 1 12 . replaceAt 2 2
--solve1 = head . run

solve2 :: Problem -> Answer
solve2 input =
    let value = 19690720
        good_programs =
            [ (noun, verb) | noun <- [0..99]
                           , verb <- [0..99]
                           , program <- [replaceAt 1 noun . replaceAt 2 verb $ input]
                           , head (run program) == value]
    in f $ head good_programs
    where
        f (noun, verb) = 100 * noun + verb