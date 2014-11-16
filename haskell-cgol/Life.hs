module Life (glider, acorn, nextGeneration, Location(Location), Life.div) where

import qualified Data.Set as Set
import qualified Data.Foldable as Foldable

data Location = Location Int Int deriving (Ord)
type Universe = Set.Set Location

instance (Eq Location) where
  (Location x1 y1) == (Location x2 y2) = x1 == x2 && y1 == y2 

instance (Show Location) where
  show (Location x y) = "(" ++ (show x) ++ ", " ++ (show y) ++ ")"

instance (Num Location) where
  (Location a b) + (Location c d) = (Location (a+c) (b+d))
  (Location a b) * (Location c d) = (Location (a*c) (b*d))
  signum (Location a b) = (Location (signum a) (signum b))
  abs (Location a b) = (Location (abs a) (abs b))
  fromInteger i = (Location (fromInteger i) (fromInteger i))

div (Location a b) (Location c d) = (Location (fromInteger $ toInteger $ a `Prelude.div` c) (fromInteger $ toInteger $ b `Prelude.div` d))

glider = Set.fromList [
    (Location 1 (-1)),
    (Location 0 (-1)),
    (Location (-1) (-1)),
    (Location 1 0),
    (Location 0 1)
  ]

acorn = Set.fromList [
    (Location 0 0),
    (Location (-1) 1),
    (Location (-1) 2),
    (Location (-1) 3),

    (Location (-1) (-2)),
    (Location (-1) (-3)),
    (Location 1 (-2))
  ]

nextGeneration universe = keepLivingBasedOn universe $ (adjacentTo universe) `Set.union` universe 

keepLivingBasedOn :: Universe -> Universe -> Universe
keepLivingBasedOn previousGeneration = Set.filter (isAliveGiven previousGeneration) 

adjacentTo :: Universe -> Universe
adjacentTo universe = Set.fold (Set.union) Set.empty $ Set.map (adjacentToSingle) universe

adjacentToSingle :: Location -> Universe
adjacentToSingle location = Set.fromList [
    shift 0 1 location,
    shift 1 0 location,
    shift 1 1 location,
    shift 0 (-1) location,
    shift (-1) 0 location,
    shift (-1) (-1) location,
    shift (-1) 1 location,
    shift 1 (-1) location
  ]

shift :: Int -> Int -> Location -> Location
shift up left (Location x y) = Location (x + up) (y + left)

isAliveGiven :: Universe -> Location -> Bool
isAliveGiven generation location = determineLifeRulingFor (livingNeighbors location generation) (isAlive location generation)

livingNeighbors :: Location -> Universe -> Int
livingNeighbors location generation = Set.size $ (adjacentToSingle location) `Set.intersection` generation

isAlive = Set.member 

determineLifeRulingFor neighbors liveness
-- Any live cell with fewer than two live neighbours dies, as if caused by under-population.
  | neighbors < 2 && liveness = False
-- Any live cell with two or three live neighbours lives on to the next generation.
  | (neighbors == 2 || neighbors == 3) && liveness = True
-- Any live cell with more than three live neighbours dies, as if by overcrowding.
  | neighbors > 3 && liveness = False
-- Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
  | neighbors == 3 && not liveness = True
-- otherwise it is dead
  | otherwise = False
