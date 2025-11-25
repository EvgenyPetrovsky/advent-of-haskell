module Y19D07
    ( parse
    , solve1
    , solve2
    ) where

import qualified Y19.IntCodeComputer as ICC
import Data.List (permutations)
import AOCutil ((|>))

type Problem = ICC.Program
type Answer = Int

parse :: String -> Problem
parse = ICC.parseInput


processThrough :: ICC.Computer -> [Int] -> Int -> Int
processThrough computer phases input =
    foldl (
        \input_signal phase_setting -> [phase_setting, input_signal]
            |> ICC.load_ins computer
            |> ICC.run_comp
            |> ICC.read_out) input phases 

solve1 :: Problem -> Answer
solve1 program = 
    let computer = ICC.init program -- :: ICC.Computer
        input_signal = 0
        phase_ins_combs = permutations [0..4]
        all_possible_outputs = map (\pis -> processThrough computer pis input_signal) phase_ins_combs
    in maximum all_possible_outputs



solve2 :: Problem -> Answer
solve2 = undefined
