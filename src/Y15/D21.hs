module Y15.D21
    ( parse
    , solve1
    , solve2
    ) where
import Data.Maybe (isNothing)

data Stats = Stats
    { health_score :: Int
    , damage_score :: Int
    , armor_score  :: Int }


data Result = Win | Loss


data Weapon = Dagger | Shortsword | Warhammer | Longsword | Greataxe
data Armor  = Leather | Chainmail | Splintmail | Bandedmail | Platemail
data Ring   = Damage1 | Damage2 | Damage3 | Defence1 | Defence2 | Defence3
  deriving (Eq, Ord)


class Wearable w where
  damage :: w -> Int
  armor  :: w -> Int
  price  :: w -> Int


instance Wearable Weapon where
  damage Dagger     = 4
  damage Shortsword = 5
  damage Warhammer  = 6
  damage Longsword  = 7
  damage Greataxe   = 8
  armor _ = 0
  price Dagger      =  8
  price Shortsword  = 10
  price Warhammer   = 25
  price Longsword   = 40
  price Greataxe    = 74

instance Wearable Armor where
  damage _ = 0
  armor Leather     = 1
  armor Chainmail   = 2
  armor Splintmail  = 3
  armor Bandedmail  = 4
  armor Platemail   = 5

  price Leather     =  13
  price Chainmail   =  31
  price Splintmail  =  53
  price Bandedmail  =  75
  price Platemail   = 102

instance Wearable Ring where
  damage Damage1    = 1
  damage Damage2    = 2
  damage Damage3    = 3
  damage _          = 0
  armor Defence1    = 1
  armor Defence2    = 2
  armor Defence3    = 3
  armor _           = 0

  price Damage1     =  25
  price Damage2     =  50
  price Damage3     = 100
  price Defence1    =  20
  price Defence2    =  40
  price Defence3    =  80

--type Ring1 = Ring
--type Ring2 = Ring

type Inventory =
  ( Weapon
  , Maybe Armor
  , Maybe Ring
  , Maybe Ring
  )

allInvCombs :: [Inventory]
allInvCombs =
  [ (w, a, r1, r2) |
      w <- [Dagger, Shortsword, Warhammer, Longsword, Greataxe],
      a <-  Nothing : map Just [Leather, Chainmail, Splintmail, Bandedmail, Platemail],
      r1 <- Nothing : map Just [Damage1, Damage2, Damage3, Defence1, Defence2, Defence3],
      r2 <- Nothing : map Just [Damage1, Damage2, Damage3, Defence1, Defence2, Defence3],
      r1 < r2 || isNothing r1 && isNothing r2
  ]

type Problem = Stats

type Answer = Int

playerStats :: Inventory -> Stats
playerStats (w, a, r1, r2) =
  Stats
    { health_score = 100
    , damage_score = damage w + maybe 0 damage r1 + maybe 0 damage r2
    , armor_score  = maybe 0 armor a  + maybe 0 armor r1  + maybe 0 armor r2
    }

inventoryCost :: Inventory -> Int
inventoryCost (w, a, r1, r2) = price w + maybe 0 price a + maybe 0 price r1 + maybe 0 price r2

{- number of hits player 1 should make to kill player 2 -}
hitsToEnd :: Stats -> Stats -> Int
hitsToEnd p1 p2 =
  let dmg = damage_score p1
      hlt = health_score p2
      arm = armor_score  p2
  -- this is rounding to upper integer trick
      impact = max (dmg - arm) 1
  in
    (hlt + impact - 1) `div` impact


fastforward :: Stats -> Stats -> Result
fastforward enemy player =
  let hits2win  = hitsToEnd player enemy
      hits2lose = hitsToEnd enemy player
  in if hits2win <= hits2lose then Win else Loss


parse :: String -> Problem
parse _ = Stats 109 8 2


-- find minimum amount of money to beat the enemy
solve1 :: Problem -> Answer
solve1 enemy =
  let all_inventory_combinations = allInvCombs
      all_inventory_costs = map inventoryCost all_inventory_combinations
      all_player_stats = map playerStats all_inventory_combinations
      all_results = map (fastforward enemy) all_player_stats
  in minimum . map fst . filter (filter_win . snd) $ zip all_inventory_costs all_results
  where
    filter_win :: Result -> Bool
    filter_win Win  = True
    filter_win Loss = False


solve2 :: Problem -> Answer
solve2 enemy =
  let all_inventory_combinations = allInvCombs
      all_inventory_costs = map inventoryCost all_inventory_combinations
      all_player_stats = map playerStats all_inventory_combinations
      all_results = map (fastforward enemy) all_player_stats
  in maximum . map fst . filter (not . filter_win . snd) $ zip all_inventory_costs all_results
  where
    filter_win :: Result -> Bool
    filter_win Win  = True
    filter_win Loss = False
