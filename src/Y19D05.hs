{-# LANGUAGE InstanceSigs #-}

module Y19D05
    ( parse
    , solve1
    , solve2
    ) where

type Problem = [Int]
type Answer = Int

type Program = [Int]
type Memory = [Int]

type Pointer = Int


data Value =
      Position Pointer
    | Immediate Int
    deriving Show

getValue :: Memory -> Value -> Int
getValue _ (Immediate x) = x
getValue m (Position r) = m !! r
--getValue _ v = error $ "invalid value kind: " ++ show v


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
  deriving Show

isHalt :: Instruction -> Bool
isHalt OpExit = True
isHalt _      = False


class IntCodeComputer c where
    start :: Program -> c
    load_ins :: c -> [Int] -> c
    read_out :: c -> Int
    read_ins :: c -> Instruction
    --move_ptr :: c -> c
    exec_ins :: c -> Instruction -> c
    run_comp :: c -> c
    run_comp c
        | isHalt i = c
        | otherwise = run_comp . exec_ins c $ i
        where
            i = read_ins c

data Computer = Computer {
      pointer :: Pointer
    , memory  :: Memory
    , input   :: [Int]
    , output  :: [Int]
} deriving Show

instance IntCodeComputer Computer where

    start :: Program -> Computer
    start program = Computer { pointer = 0, memory = program, input = [], output = [] }

    load_ins :: Computer -> [Int] -> Computer
    load_ins c ins = c {input = ins}

    read_out :: Computer -> Int
    read_out = last . output

    read_ins :: Computer -> Instruction
    read_ins Computer {pointer=_p , memory=_m} =
        case opcode `mod` 100 of
            1 -> OpAdd (param_val 1 v1) (param_val 2 v2) (param_val 3 v3)
            2 -> OpMul (param_val 1 v1) (param_val 2 v2) (param_val 3 v3)
            3 -> OpIn  (param_val 1 v1)
            4 -> OpOut (param_val 1 v1)
            5 -> OpJIT (param_val 1 v1) (param_val 2 v2)
            6 -> OpJIF (param_val 1 v1) (param_val 2 v2)
            7 -> OpLT  (param_val 1 v1) (param_val 2 v2) (param_val 3 v3)
            8 -> OpEq  (param_val 1 v1) (param_val 2 v2) (param_val 3 v3)
            99 -> OpExit
            x -> error $ "undefined opcode: " ++ show x ++ " at address " ++ show _p
        where
            (opcode, v1, v2, v3) = (memRead _m _p, memRead _m (_p+1), memRead _m (_p+2), memRead _m (_p+3))
            param_val :: Int -> Int -> Value
            param_val parnum val
                | adrmod == 0 = Position val
                | adrmod == 1 = Position (_p+parnum)
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




memRead :: Memory -> Pointer -> Int
memRead = (!!)
memWrite :: Int -> Memory -> Pointer -> Memory
memWrite val mem ptr = take ptr mem ++ [val] ++ drop (ptr+1) mem


parse :: String -> Problem
parse = map read . words . substitute ',' ' '
    where
        substitute :: Eq a => a -> a -> [a] -> [a]
        substitute _    _  [] = []
        substitute from to (x:xs)
          | from == x = to: substitute from to xs
          | otherwise = x : substitute from to xs


solve1 :: Problem -> Answer
solve1 p =
    let init_state = start p `load_ins` [1] :: Computer
        final_state = run_comp init_state
    in
        read_out final_state
        --error (show final_state)
        --error $ show $ output final_state--
        --error $ show (output final_state)

solve2 :: Problem -> Answer
solve2 p =
    let init_state = start p `load_ins` [5] :: Computer
        final_state = run_comp init_state
    in
        read_out final_state
        --error $ show final_state
