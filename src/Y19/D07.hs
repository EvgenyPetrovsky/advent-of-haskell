module Y19.D07
    ( parse
    , solve1
    , solve2
    ) where

import qualified Y19.IntCodeComputer as ICC
import Data.List (permutations)
import Util.Compute ((|>))

type Problem = ICC.Program
type Answer = Int

parse :: String -> Problem
parse = ICC.parseInput


processThrough1 :: ICC.Computer -> [Int] -> Int -> Int
processThrough1 computer phases input =
    foldl (
        \input_signal phase_setting -> [phase_setting, input_signal]
            |> ICC.upld_inp computer
            |> (`ICC.run_until` ICC.statusNonEmptyOut)
            |> (fst . ICC.pull_out)
        ) input phases

solve1 :: Problem -> Answer
solve1 program =
    let computer = ICC.init program -- :: ICC.Computer
        input_signal = 0
        phase_ins_combs = permutations [0..4]
        all_possible_outputs = map (\pis -> processThrough1 computer pis input_signal) phase_ins_combs
    in maximum all_possible_outputs

processThrough2 :: ICC.Computer -> [Int] -> Int -> Int
processThrough2 computer phases input =
    let calibrated = map (computer `ICC.push_in`) phases
    in go calibrated input
    where
        go :: [ICC.Computer] -> Int -> Int
        go [] signal = signal
        go comps insig =
            let (new_signal, new_comps) = foldl (
                    \(sig, cs) c ->
                        c `process_signal` sig
                        |> (\(new_c, out) -> (out, cs ++ [new_c]))
                    ) (insig, []) comps
            in go (filter (not . ICC.statusHalt) new_comps) new_signal
        process_signal :: ICC.Computer -> Int -> (ICC.Computer, Int)
        process_signal c s
            | ICC.statusHalt after_run && ICC.statusNonEmptyOut after_run = (new_c, new_out)
            | ICC.statusNonEmptyOut after_run = (new_c, new_out)
            | ICC.statusHalt after_run = (new_c, s)
            | otherwise = error $ "Undefined condition for computer " ++ show after_run
            where
                after_run = c `ICC.push_in` s `ICC.run_until` stop_condition
                (new_out, new_c) = ICC.pull_out after_run
                stop_condition :: ICC.Computer -> Bool
                stop_condition x = ICC.statusHalt x || ICC.statusNonEmptyOut x


solve2 :: Problem -> Answer
solve2 program =
    let computer = ICC.init program -- :: ICC.Computer
        input_signal = 0
        phase_ins_combs = permutations [5..9]
        all_possible_outputs = map (\pis -> processThrough2 computer pis input_signal) phase_ins_combs
    in maximum all_possible_outputs
