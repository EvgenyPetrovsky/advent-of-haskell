module Util.Graph (
    astar
) where

import qualified Data.Set as S (empty, insert, member, notMember)
import qualified Data.Map as M (empty, insert, toList, filterWithKey, lookup)


type Cost = Int

astar :: (Ord state) => state -> (state -> [(state, Cost)]) -> (state -> Bool) -> (state -> Bool) -> (state, Cost)
astar start next goal stop =
    let (discovered, _) = iter start 0 M.empty S.empty
    in head . M.toList . M.filterWithKey (\k _ -> goal k) $ discovered
    -- error "not implemented"
    where
        --next' :: (Ord state) => state -> [(state, Cost)]
        next' = filter (not . stop . fst) . next
        --iter :: (Ord state) => state -> Cost -> M.Map state Cost -> S.Set state -> (M.Map state Cost, S.Set state)
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
                    --not_a_deadend :: (Ord state) => (state, Cost) -> Bool
                    not_a_deadend (state, _) = S.notMember state deadend
                    --better_or_new :: (Ord state) => (state, Cost) -> Bool
                    better_or_new (state, path_cost) =
                        case M.lookup state visited of
                            Just known_cost -> known_cost > path_cost
                            Nothing -> True
