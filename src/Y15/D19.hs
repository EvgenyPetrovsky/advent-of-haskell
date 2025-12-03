module Y15.D19
    ( parse
    , solve1
    , solve2
    ) where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.List (nub, sortOn, isPrefixOf)
type Element = Int
type Chain = [Element]
type Replacement = (Element, Chain)

data Problem = Problem {
    fwd_replacements :: M.Map Element [Chain],
    rev_replacements :: M.Map Chain Element,
    molecule :: Chain
} deriving Show

type Answer = Int


elmap :: M.Map String Element
elmap = M.fromList $ ("e", 0) : zip (nub $ unique ++ non_unique) [1..]
    where
        unique = ["Ar", "Rn", "Y", "Th", "Si", "C"]
        non_unique = ["Al", "B", "Ca", "F", "H", "Mg", "N", "O", "P", "Si", "Th", "Ti"]

encode :: String -> [Element]
encode [] = []
encode str
    | c2 `M.member` elmap = elmap M.! c2 : encode (drop 2 str)
    | c1 `M.member` elmap = elmap M.! c1 : encode (drop 1 str)
    | otherwise = error $ "neither c1 (" ++ c1 ++ ") nor c2 (" ++ c2 ++ ") are valid elemets. "
    where
        c2 = take 2 str
        c1 = take 1 str


parseOneReplacement :: String -> Replacement
parseOneReplacement ln =
    case splitOn " => " ln of
        lhs:rhs:_ -> (head $ encode lhs, encode rhs)
        _ -> error ("incorrect replacement line: " ++ ln)


parse :: String -> Problem
parse input =
    let lns = lines input
        --in_molecule = encode "NRnFArTiRnFArPTiMg"
        in_molecule = encode $ last lns
        replacement_lns = takeWhile (/= "") lns
        parsed_replacements = map parseOneReplacement replacement_lns

    in Problem {
        fwd_replacements = foldl (\z (k, v) -> M.insertWith (++) k [v] z) M.empty parsed_replacements,
        rev_replacements = M.fromList . map (\(a,b) -> (b,a)) $ parsed_replacements,
        molecule = in_molecule
        }


allMutations :: Chain -> M.Map Element [[Element]] -> [Chain]
allMutations mol repls =
    iter [] mol
    where
        iter :: Chain -> Chain -> [Chain]
        iter _ [] = []
        iter lhs (r:rs)
          | M.member r repls = map (\x -> lhs ++ x ++ rs) (repls M.! r :: [[Element]]) ++ iter lhs' rs
          | otherwise = iter lhs' rs
            where lhs' = lhs ++ [r]


solve1 :: Problem -> Answer
solve1 Problem {molecule = m, fwd_replacements = rs} =
    length . nub . allMutations m $ rs


solve2 :: Problem -> Answer
solve2 Problem {molecule = mol, fwd_replacements = _, rev_replacements = rev} =
    greedySearch mol rev


findMatches :: Chain -> [Chain] -> [(Chain, Int)]
findMatches _ [] = []
findMatches mol (cs:css) =
    case findStart mol 0 of
        Nothing -> findMatches mol css
        --Just i  -> (cs, i) : findMatches mol css
        Just i  -> [(cs, i)]
    where
        findStart :: Chain -> Int -> Maybe Int
        findStart [] _ = Nothing
        findStart mol1 idx
            | cs `isPrefixOf` mol1 = Just idx
            | otherwise = findStart (tail mol1) (idx + 1)

greedySearch :: Chain -> M.Map Chain Element -> Int
greedySearch mol rev_map =
    let chain_idxs = findMatches mol rev_keys_desc_order
    in greedySearch' mol chain_idxs 0 9999
    where
        rev_keys_desc_order = sortOn (negate . length) . M.keys $ rev_map

        greedySearch' :: Chain -> [(Chain, Int)] -> Int -> Int -> Int
        greedySearch' [0] _ step best_result
            | step < best_result = step
            | otherwise = best_result
        greedySearch' _ [] _ best_result = best_result
        greedySearch' _mol ((chain, idx):ch_idxs) step best_result
            | step >= best_result = best_result
            | otherwise =
                let (lhs, rhs) = splitAt idx _mol
                    to = rev_map M.! chain
                    new_mol = lhs ++ [to] ++ drop (length chain) rhs
                    new_ch_idxs = findMatches new_mol rev_keys_desc_order
                    new_best_result = greedySearch' new_mol new_ch_idxs (step + 1) best_result
                in greedySearch' _mol ch_idxs step new_best_result
