module Y15D19
    ( parse
    , solve1
    , solve2
    ) where

import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (nub, sortOn, isPrefixOf)
type Element = Int
type Chain = [Element]
type Replacement = (Element, Chain)
--type Replacements = M.Map Element [[Element]]

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

decode :: [Element] -> String
decode els =
    let rev_elmap = M.fromList . map (\(a,b) -> (b,a)) . M.toList $ elmap
    in concatMap (rev_elmap M.!) els


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


-- find all possible previous states for molecule - this is inverse mutation operation to get from current state to all possible previous states.
-- we need to identify all entries of Chains from replacements and substiture them with original values.
allReverses :: Chain -> M.Map Chain Element -> [Chain]
allReverses mol rev_map =
    iter [] mol
    where
        -- sorted by len desc
        --m_keys = sortOn (negate . length) $ M.keys rev_map
        -- improve by using findIndices and starting with longest subchains, include the len of subchain (len k) and element (v) into find results to easier operate on result
        --replacements = concatMap (\k -> map (\idx -> (idx, length k, rev_map M.! k)) $ findIndices (\_ -> error "it is working on element and we need it on subsequence") mol) m_keys

        -- than do (a, temp_b) = (splitAt index); b = drop (len_k) temp_b; reversed chain = a ++ [v] ++ b
        iter :: Chain -> Chain -> [Chain]
        iter _ [] = []
        iter lhs rhs@(r:rs) =
            let all_reductions :: [Chain]
                all_reductions = map (\x -> lhs ++ [rev_map M.! x] ++ drop (length x) rhs) . filter (`isPrefixOf` rhs) $ M.keys rev_map
            in all_reductions ++ iter (lhs ++ [r]) rs


solve1 :: Problem -> Answer
solve1 Problem {molecule = m, fwd_replacements = rs} =
    length . nub . allMutations m $ rs


solve2 :: Problem -> Answer
solve2 Problem {molecule = mol, fwd_replacements = _, rev_replacements = rev} =
    let

        {-
        next :: Chain -> [(Chain, Int)]
        next s = map (\x -> (x, 1)) . nub $ allMutations s rs

        stop :: Chain -> Bool
        stop s
          | length s > length mol = True
          -- | or . M.elems . M.mapWithKey (\k v -> countElements k growing > v) $ maxElCounts = True
          | otherwise = False

        goal :: State -> Bool
        goal = mol ==

        start = elmap M.! "e"
        -}

        rev_next :: Chain -> [(Chain, Int)]
        rev_next s = map (\x -> (x, 1)) . sortOn length . nub $ allReverses s rev

        rev_stop :: Chain -> Bool
        rev_stop _ = False

        rev_goal :: Chain -> Bool
        rev_goal = (==) [elmap M.! "e"]

        rev_start = mol

    in
        -- forward
        --snd $ astar [start] next goal stop
        -- reverse
        --snd $ astar rev_start rev_next rev_goal rev_stop
        -- greedy
        greedySearch mol rev



-- GRAPH algorithms
type State = [Element]
type Cost = Int

astar :: State -> (State -> [(State, Cost)]) -> (State -> Bool) -> (State -> Bool) -> (State, Cost)
astar start next goal stop =
    let (discovered, _) = iter start 0 M.empty S.empty
    in head . M.toList . M.filterWithKey (\k _ -> goal k) $ discovered
    -- error "not implemented"
    where
        next' :: State -> [(State, Cost)]
        next' = filter (not . stop . fst) . next
        iter :: State -> Cost -> M.Map State Cost -> S.Set State -> (M.Map State Cost, S.Set State)
        iter cur_state acc_cost visited deadend
          | goal cur_state = (visited, deadend)
          | cur_state `S.member` deadend = (visited, deadend)
          | stop cur_state = (visited, S.insert cur_state deadend)
          | otherwise =
                let next_states = map (\(s,c) -> (s,c+acc_cost)) $ next' cur_state
                    no_deadends = filter not_a_deadend next_states
                    must_visit  = filter better_or_new no_deadends
                    upd_deadend = if null must_visit then S.insert cur_state deadend else deadend
                in
                    foldl (\(v, d) (s,c) -> iter s c (M.insert s c v) d) (visited, upd_deadend) must_visit
                where
                    not_a_deadend :: (State, Cost) -> Bool
                    not_a_deadend (state, _) = S.notMember state deadend
                    better_or_new :: (State, Cost) -> Bool
                    better_or_new (state, path_cost) =
                        case M.lookup state visited of
                            Just known_cost -> known_cost > path_cost
                            Nothing -> True

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
        greedySearch' mol ((chain, idx):ch_idxs) step best_result
            | step >= best_result = best_result
            | otherwise =
                let (lhs, rhs) = splitAt idx mol
                    to = rev_map M.! chain
                    new_mol = lhs ++ [to] ++ drop (length chain) rhs
                    new_ch_idxs = findMatches new_mol rev_keys_desc_order
                    new_best_result = greedySearch' new_mol new_ch_idxs (step + 1) best_result
                in greedySearch' mol ch_idxs step new_best_result
