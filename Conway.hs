module Conway (fromList, nextgen, survives, Cell, Point) where

import qualified Data.Set as Set
import Data.Set (union, intersection, member)


-- Public API.
-- ---------------------------------------------------------------------------

type Cell = (Point, Point)
type Point = Int

-- | Construct a Set of @Cell@s from a 2d list of 1's (living cells)
-- and 0's (dead cells). 
fromList :: [[Int]] -> Set.Set Cell
fromList rows = Set.fromList $ concat cells
    where cells = [[(x, y) | (x, cell) <- iterIndices row, cell /= 0]
                   | (y, row) <- iterIndices rows]

-- | Generate the next iteration of the Game of Life.
nextgen :: Set.Set Cell -> Set.Set Cell
nextgen cells = foldr keepSurvivors Set.empty (withAdjacent cells)
    where keepSurvivors cell newCells = if cell `survives` cells
                                        then Set.insert cell newCells
                                        else newCells

-- | Return True if, given a @Cell@ and a Set of @Cell@s,
-- the given cell survives the next iteration.
survives :: Cell -> Set.Set Cell -> Bool
survives cell cells
  | liveNeighbors == 3 = True
  | liveNeighbors == 2 = cell `member` cells
  | otherwise          = False
  where
      neighbors = adjacentCells cell
      liveNeighbors = length (cells `intersection` neighbors)


-- Private API.
-- ---------------------------------------------------------------------------

-- The union of a Set of @Cell@s with the Set of all its adjacent cells.
withAdjacent :: Set.Set Cell -> Set.Set Cell
withAdjacent = foldr f Set.empty
    where f c cs = Set.insert c cs `union` adjacentCells c

-- The @Set@ of all adjacent @Cell@s to a given @Cell@.
adjacentCells :: Cell -> Set.Set Cell
adjacentCells cell = Set.map (vPlus cell) dirs
    where (x1, y1) `vPlus` (x2, y2) = (x1 + x2, y1 + y2)
          dirs = Set.fromList [(x, y) | x <- ds, y <- ds, x /= 0 || y /= 0]
          ds = [(-1) .. 1]

-- The list of pairs (i, x) where each `i' is the index of each item `x'
-- in the given list.
iterIndices :: (Enum n, Num n) => [a] -> [(n, a)]
iterIndices xs = zip [0..] xs
