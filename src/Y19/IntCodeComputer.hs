{-# LANGUAGE InstanceSigs #-}

module Y19.IntCodeComputer
    ( Program
    , parseInput
    , IntCodeComputer (..)
    , Computer ()
    , statusHalt
    , statusNonEmptyOut
    , statusReadEmptyIn
    ) where

import Data.Map (Map)
import qualified Data.Map as M ((!?), insert, fromList)
import Data.Maybe (fromMaybe)

type Program = [Int]
--type MemoryV1old = [Int]
type Memory = Map Int Int

type Pointer = Int


data Value = Position Pointer | Immediate Int deriving Show


getValue :: Memory -> Value -> Int
getValue _ (Immediate x) = x
getValue m (Position r) = memRead m r


data Instruction =
    OpExit
  | OpAdd Value Value Value
  | OpMul Value Value Value
  | OpIn  Value
  | OpOut Value
  | OpJIT Value Value
  | OpJIF Value Value
  | OpLT  Value Value Value
  | OpEq  Value Value Value
  | OpRBO Value
  deriving Show


class IntCodeComputer c where
    init      :: Program -> c
    upld_inp  :: c -> [Int] -> c
    push_in   :: c -> Int -> c
    pull_out  :: c -> (Int, c)

    read_ins  :: c -> Instruction
    exec_ins  :: c -> Instruction -> c

    run_until :: c -> (c -> Bool) -> c
    run_until c f
        | f c = c
        | otherwise = (`run_until` f) . exec_ins c $ read_ins c


data Computer = Computer {
      pointer :: Pointer
    , memory  :: Memory
    , input   :: [Int]
    , output  :: [Int]
    , relbase :: Pointer
} deriving Show

instance IntCodeComputer Computer where


    init :: Program -> Computer
    init program = Computer { pointer = 0, memory = loadProgram program, input = [], output = [] , relbase = 0}


    upld_inp :: Computer -> [Int] -> Computer
    upld_inp c ins = c {input = ins}


    push_in :: Computer -> Int -> Computer
    push_in c v =
        let _input = input c
        in c {input = _input ++ [v]}


    pull_out :: Computer -> (Int, Computer)
    pull_out c = case output c of
        o:os -> (o, c {output = os} )
        _ -> error $ "Nothing to pull from computer " ++ show c


    read_ins :: Computer -> Instruction
    read_ins Computer {pointer=_p , memory=_m, relbase=_b} =
        case opcode `mod` 100 of
            1 -> OpAdd (param_val 1 v1) (param_val 2 v2) (param_val 3 v3)
            2 -> OpMul (param_val 1 v1) (param_val 2 v2) (param_val 3 v3)
            3 -> OpIn  (param_val 1 v1)
            4 -> OpOut (param_val 1 v1)
            5 -> OpJIT (param_val 1 v1) (param_val 2 v2)
            6 -> OpJIF (param_val 1 v1) (param_val 2 v2)
            7 -> OpLT  (param_val 1 v1) (param_val 2 v2) (param_val 3 v3)
            8 -> OpEq  (param_val 1 v1) (param_val 2 v2) (param_val 3 v3)
            9 -> OpRBO (param_val 1 v1)
            99 -> OpExit
            x -> error $ "undefined opcode: " ++ show x ++ " at address " ++ show _p
        where
            (opcode, v1, v2, v3) = (memRead _m _p, memRead _m (_p+1), memRead _m (_p+2), memRead _m (_p+3))
            param_val :: Int -> Int -> Value
            param_val parnum val
                | adrmod == 0 = Position val         -- position mode
                | adrmod == 1 = Position (_p+parnum) -- this is literal mode but we translate it to position of literal
                | adrmod == 2 = Position (_b+val)    -- relative base mode
                | otherwise   = error $ "unknown address mode for operation " ++ show opcode
                where
                    adrmod = opcode `div` (10^(parnum+1)) `mod` 10


    exec_ins :: Computer -> Instruction -> Computer
    exec_ins c OpExit = c
    exec_ins c (OpAdd in1 in2 (Position outref)) =
        let (ptr, mem) = (pointer c, memory c)
            (in1val, in2val) = (getValue mem in1, getValue mem in2)
            outval = in1val + in2val
            newptr = ptr + 4
        in c{pointer = newptr, memory = memWrite outval mem outref}
    exec_ins _ OpAdd {} = error "not valid instruction"
    exec_ins c (OpMul in1 in2 (Position outref)) =
        let (ptr, mem) = (pointer c, memory c)
            (in1val, in2val) = (getValue mem in1, getValue mem in2)
            outval = in1val * in2val
            newptr = ptr + 4
        in c{pointer = newptr, memory = memWrite outval mem outref}
    exec_ins _ OpMul{} = error "not valid instruction"
    exec_ins c (OpIn (Position outref)) =
        let (ptr, mem) = (pointer c, memory c)
            ins = input c
            (outval, inrem) = (head ins, tail ins)
            newptr = ptr + 2
        in c{pointer = newptr, memory = memWrite outval mem outref, input = inrem}
    exec_ins _ OpIn{} = error "not valid instruction"
    exec_ins c (OpOut in1) =
        let (ptr, mem) = (pointer c, memory c)
            in1val = getValue mem in1
            outs = output c
            newptr = ptr + 2
        in c{pointer = newptr, output = outs ++ [in1val]}
    exec_ins c (OpJIT in1 in2) =
        let (ptr, mem) = (pointer c, memory c)
            (in1val, in2val) = (getValue mem in1, getValue mem in2)
            newptr = if in1val /= 0 then in2val else ptr + 3
        in c{pointer = newptr}
    exec_ins c (OpJIF in1 in2) =
        let (ptr, mem) = (pointer c, memory c)
            (in1val, in2val) = (getValue mem in1, getValue mem in2)
            newptr = if in1val == 0 then in2val else ptr + 3
        in c{pointer = newptr}
    exec_ins c (OpLT in1 in2 (Position outref)) =
        let (ptr, mem) = (pointer c, memory c)
            (in1val, in2val) = (getValue mem in1, getValue mem in2)
            outval = if in1val < in2val then 1 else 0
            newptr = ptr + 4
        in c{pointer = newptr, memory = memWrite outval mem outref}
    exec_ins _ OpLT{} = error "not valid instruction"
    exec_ins c (OpEq in1 in2 (Position outref)) =
        let (ptr, mem) = (pointer c, memory c)
            (in1val, in2val) = (getValue mem in1, getValue mem in2)
            outval = if in1val == in2val then 1 else 0
            newptr = ptr + 4
        in c{pointer = newptr, memory = memWrite outval mem outref}
    exec_ins _ OpEq{} = error "not valid instruction"
    exec_ins c (OpRBO in1) =
        let (ptr, mem) = (pointer c, memory c)
            cur    = relbase c
            in1val = getValue mem in1
            newptr = ptr + 2
        in c{pointer = newptr, relbase = cur + in1val}


statusHalt :: Computer -> Bool
statusHalt c = case read_ins c of
    OpExit -> True
    _ -> False

statusReadEmptyIn :: Computer -> Bool
statusReadEmptyIn c = case (read_ins c, input c) of
        (OpIn _, []) -> True
        _ -> False


statusNonEmptyOut :: Computer -> Bool
statusNonEmptyOut = not . null . output


memRead :: Memory -> Pointer -> Int
memRead mem ptr = fromMaybe 0 $ mem M.!? ptr
-- memRead m p
--     | p >= length m = m !! (p `mod` length m)
--     | p < 0 = error "Can't read from negative address"
--     | otherwise = m !! p




memWrite :: Int -> Memory -> Pointer -> Memory
memWrite val mem ptr = M.insert ptr val mem
--memWrite val mem ptr = take ptr mem ++ [val] ++ drop (ptr+1) mem

loadProgram :: Program -> Memory
loadProgram program = M.fromList $ zip [0..] program

parseInput :: String -> Program
parseInput = map read . words . substitute ',' ' '
    where
        substitute :: Eq a => a -> a -> [a] -> [a]
        substitute _    _  [] = []
        substitute from to (x:xs)
          | from == x = to: substitute from to xs
          | otherwise = x : substitute from to xs