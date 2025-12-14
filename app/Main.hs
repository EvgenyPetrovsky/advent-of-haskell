module Main (main) where

import Y25.D12 (
    parse
  , solve1
  , solve2
  )

main :: IO ()
main = do
    input <- getContents
    let problem = parse input
    putStrLn $ "Answer part 1: " ++ (show . solve1 $ problem)
    putStrLn $ "Answer part 2: " ++ (show . solve2 $ problem)
