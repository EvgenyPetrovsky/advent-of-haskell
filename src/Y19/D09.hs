module Y19.D09
    ( parse
    , solve1
    , solve2
    ) where

import qualified Y19.IntCodeComputer as ICC

type Problem = ICC.Program
type Answer = Int

parse :: String -> Problem
parse = ICC.parseInput


solve1 :: Problem -> Answer
solve1 program = 
    let input_value = 1 :: Int
        comp = (`ICC.push_in` input_value) $ ICC.init program :: ICC.Computer
    in fst . ICC.pull_out $ ICC.run_until comp ICC.statusHalt


solve2 :: Problem -> Answer
solve2 program = 
    let input_value = 2 :: Int
        comp = (`ICC.push_in` input_value) $ ICC.init program :: ICC.Computer
    in fst . ICC.pull_out $ ICC.run_until comp ICC.statusHalt
