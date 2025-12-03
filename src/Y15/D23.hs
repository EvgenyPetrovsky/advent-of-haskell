module Y15.D23
    ( parse
    , solve1
    , solve2
    ) where

type Problem = [Instruction]

type Answer = Int

type Register = Int
data RegAdr = RegA | RegB
type Offset = Int

data Instruction =
    HLF RegAdr |
    TPL RegAdr |
    INC RegAdr |
    JMP Offset |
    JIE RegAdr Offset |
    JIO RegAdr Offset


type RegisterState = (Register, Register)

parseInstruction :: String -> Instruction
parseInstruction instruction_line
    | ins == "hlf" = HLF reg_nm
    | ins == "tpl" = TPL reg_nm
    | ins == "inc" = INC reg_nm
    | ins == "jmp" = JMP jump_offset
    | ins == "jie" = JIE reg_nm jump_if_offset
    | ins == "jio" = JIO reg_nm jump_if_offset
    | otherwise = error ("instruction is not recognised: " ++ instruction_line)
        where
            (ins, rest_i) = splitAt 3 instruction_line
            reg_nm :: RegAdr
            reg_nm = case take 1 $ drop 1 rest_i of
                "a" -> RegA
                "b" -> RegB
                _ -> error ("unknown register: " ++ take 1 rest_i ++ " in instruction: " ++ instruction_line)
            jump_if_offset = read . dropWhile (== '+') . drop 4 $ rest_i
            jump_offset = read . dropWhile (== '+') . drop 1 $ rest_i

parse :: String -> Problem
parse = map parseInstruction . lines

run :: RegisterState -> [Instruction] -> RegisterState
run start instructions =
    iter start 0
    where
        len = length instructions
        iter :: RegisterState -> Int -> RegisterState
        iter state@(ra, rb) address
            | address >= len = state
            | otherwise = case instructions !! address of
                HLF RegA -> iter (ra `div` 2, rb) next
                HLF RegB -> iter (ra, rb `div` 2) next
                TPL RegA -> iter (ra * 3, rb) next
                TPL RegB -> iter (ra, rb * 3) next
                INC RegA -> iter (ra + 1, rb) next
                INC RegB -> iter (ra, rb + 1) next
                JMP offset -> iter state (address + offset)
                JIE RegA offset -> if even ra then iter state (over offset) else iter state next
                JIE RegB offset -> if even rb then iter state (over offset) else iter state next
                JIO RegA offset -> if 1 == ra then iter state (over offset) else iter state next
                JIO RegB offset -> if 1 == ra then iter state (over offset) else iter state next
            where
                next = address + 1
                over = (+) address


solve1 :: Problem -> Answer
solve1 instructions =
    let (_, reg_b_value) = run (0,0) instructions
    in reg_b_value

solve2 :: Problem -> Answer
solve2 instructions =
    let (_, reg_b_value) = run (1,0) instructions
    in reg_b_value
