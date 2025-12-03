{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Y15.D22
    ( parse
    , solve1
    , solve2
    ) where
import qualified Data.Map as M
import Data.Maybe (fromMaybe)


data Stats = Stats {
    health_score :: Int
  , damage_score :: Int
  , armor_score  :: Int
  , mana_score   :: Int
} deriving (Show)

type Epoch = Int
data Spell = MagicMissile | Drain | Shield | Poison | Recharge
  deriving (Eq, Ord, Show)
data Turn = Player | Enemy deriving (Eq, Show)

cost :: Spell -> Int
cost MagicMissile = 53
cost Drain = 73
cost Shield = 113
cost Poison = 173
cost Recharge = 229

duration :: Spell -> Int
duration Shield = 6
duration Poison = 6
duration Recharge = 5
duration _ = 0

damage :: Spell -> Int
damage MagicMissile = 4
damage Drain = 2
damage Poison = 3
damage _ = 0

health :: Spell -> Int
health Drain = 2
health _ = 0

armor :: Spell -> Int
armor Shield = 7
armor _ = 0

mana :: Spell -> Int
mana Recharge = 101
mana _ = 0

enemyEffect :: Spell -> Stats -> Stats
enemyEffect spell _enemy@Stats{health_score=_hs} =
  _enemy { health_score = _hs - damage spell }

playerEffect :: Spell -> Stats -> Stats
playerEffect spell _player@Stats{health_score=_hs, armor_score=_as, mana_score=_ms} =
  _player
    { health_score = _hs + health spell
    , armor_score  = _as + armor spell
    , mana_score   = _ms + mana spell }

data Decision = Player_Dies | Player_Skips | Enemy_Dies | Enemy_Hits | Player_Casts Spell deriving (Show)

data State = State
  { player :: Stats
  , enemy :: Stats
  , epoch :: Epoch
  , result :: Result
  , spells :: M.Map Spell Epoch
  , turn :: Turn
  , mana_spent :: Int
  , history :: [(Epoch, Decision)]
  } deriving (Show)

data Result = Win | Loss | Unknown deriving (Show)

type Problem = (Stats, Stats)

type Answer = Int

--------------------------

play1turn :: State -> [State]
play1turn in_state@State{player=ps, enemy=es, epoch = _time, spells=ss, turn=_turn, mana_spent=_mana_spent, history=_history} =
  let
    new_time = _time + 1
    active  = M.keys $ M.filter (>= new_time) ss
    player1 = foldr playerEffect (ps {damage_score = 0, armor_score = 0}) active
    enemy1  = foldr enemyEffect  es active
    spells_to_cast =
      filter (\s -> fromMaybe new_time (ss M.!? s) <= new_time) .
      filter (\s -> mana_score player1 >= cost s) $
      [MagicMissile, Drain, Shield, Poison, Recharge]
    player2 = case _turn of
      Player -> player1
      Enemy  -> player1 { health_score = health_score player1 - max (damage_score es - armor_score player1) 1}
  in
    if new_time > 50
    then error $ show in_state
    else if health_score es <= 0
    then [in_state {result = Win, history = (new_time, Enemy_Dies) : _history}]
    else if health_score ps <= 0
    then [in_state {result = Loss, history = (new_time, Player_Dies) : _history}]
    else if health_score enemy1 <= 0
    then [in_state {player = player1, enemy = enemy1, epoch = new_time, result = Win, history = (new_time, Enemy_Dies) : _history}]
    else if _turn == Player && null spells_to_cast
    then [in_state {player = player2, enemy = enemy1, turn = Enemy, epoch = new_time, history = (new_time, Player_Skips) : _history}]
    else if _turn == Enemy && health_score player2 <= 0
    then [in_state {result = Loss, history = (new_time, Player_Dies) : _history}]
    else if _turn == Enemy
    then [in_state {player = player2, enemy = enemy1, turn = Player, epoch = new_time, history = (new_time, Enemy_Hits) : _history}]
    else map (
      \spell -> spell `cast` in_state {player = player2, enemy = enemy1, turn = Enemy, epoch = new_time, history = (new_time, Player_Casts spell) : _history}
      ) spells_to_cast
  where
    cast :: Spell -> State -> State
    cast _spell _state =
      case _spell of
        MagicMissile -> _spell `immediate_effect` updated_state
        Drain        -> _spell `immediate_effect` updated_state
        _            -> updated_state
      where
        immediate_effect par_spell par_state =
          par_state
            { player = playerEffect par_spell (player par_state)
            , enemy = enemyEffect par_spell (enemy par_state) }
        updated_state =
          _state
            { player = (player _state) {mana_score = mana_score (player _state) - cost _spell}
            , spells = M.insert _spell (epoch _state + duration _spell) (spells _state)
            , mana_spent = mana_spent _state + cost _spell
            }

run :: State -> Answer
run start_state =
  let (spent_mana, _) = go (1700, start_state) start_state
  in spent_mana
  where
    go :: (Int, State) -> State -> (Int, State)
    go best state =
      let new_states = play1turn state
          win_states = filter (is_win . result) new_states
          new_best   = foldl (\z s -> min' z (mana_spent s, s)) best win_states
          nxt_states = filter (\s -> fst new_best >= mana_spent s) . filter (is_unknown . result) $ new_states
      in foldl min' new_best $ map (go new_best) nxt_states
      where
        is_win :: Result -> Bool
        is_win Win = True
        is_win _   = False
        is_unknown :: Result -> Bool
        is_unknown Unknown = True
        is_unknown _       = False
        min' :: Ord a => (a,b) -> (a,b) -> (a,b)
        min' v1@(a1, _) v2@(a2, _)
          | a2 < a1   = v2
          | otherwise = v1

run2 :: State -> Answer
run2 start_state =
  let (spent_mana, actions) = go (1700, start_state) start_state
  in error $ "Mana spent: " ++ show spent_mana ++ ". Actions: " ++ (show . reverse . history $ actions)
  where
    go :: (Int, State) -> State -> (Int, State)
    go best state =
      let new_states = play1turn . steal_player_health $ state
          win_states = filter (is_win . result) new_states
          new_best   = foldl (\z s -> min' z (mana_spent s, s)) best win_states
          nxt_states = filter (\s -> fst new_best >= mana_spent s) . filter (is_unknown . result) $ new_states
      in foldl min' new_best $ map (go new_best) nxt_states
      where
        is_win :: Result -> Bool
        is_win Win = True
        is_win _   = False
        is_unknown :: Result -> Bool
        is_unknown Unknown = True
        is_unknown _       = False
        min' :: Ord a => (a,b) -> (a,b) -> (a,b)
        min' v1@(a1, _) v2@(a2, _)
          | a2 < a1   = v2
          | otherwise = v1
        steal_player_health :: State -> State
        steal_player_health par_state@State{player=_player@Stats{health_score=_health}, turn=_turn} =
          case _turn of
            Player -> par_state {player = _player{health_score=_health-1}}
            Enemy  -> par_state


parse :: String -> Problem
parse _ =
  let
    player_stats = Stats 50 0 0 500
    --player_stats = Stats 10 0 0 250
    enemy_stats = Stats 58 9 0 0
    --enemy_stats = Stats 14 8 0 0
  in (player_stats, enemy_stats)

solve1 :: Problem -> Answer
solve1 (player_stats, enemy_stats) =
  run init_state
  where
    init_spells  = M.empty
    init_state = State player_stats enemy_stats 0 Unknown init_spells Player 0 []

solve2 :: Problem -> Answer
solve2 (player_stats, enemy_stats) =
  run2 init_state
  where
    init_spells  = M.empty
    init_state = State player_stats enemy_stats 0 Unknown init_spells Player 0 []
