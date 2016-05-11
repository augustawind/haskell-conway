module Conway (fromList, nextgen, survives, World, Cell, Point) where

import qualified Data.Set as Set
import Data.Set (union, intersection, member)


-- Public API.
-- ---------------------------------------------------------------------------

type World = Set.Set Cell
type Cell = (Point, Point)
type Point = Int

-- | Construct a World from a 2d list of 1's (living cell) and 0's (dead cell). 
fromList :: [[Int]] -> World
fromList rows = Set.fromList $ concat cells
    where cells = [[(x, y) | (x, cell) <- iterIndices row, cell /= 0]
                    | (y, row) <- iterIndices rows]

-- | Generate the next iteration of the Game of Life.
nextgen :: World -> World
nextgen world = foldr f Set.empty (withAdjacent world)
    where f cell cells = if cell `survives` world
                              then Set.insert cell cells
                              else cells 

-- | Return True if, given a Cell and World, the given cell survives the
-- | next iteration.
survives :: Cell -> World -> Bool
survives cell world
  | liveNeighbors == 3 = True
  | liveNeighbors == 2 = if cell `member` world
                            then True
                            else False
  | otherwise = False
    where neighbors = adjacentCells cell
          liveNeighbors = length (world `intersection` neighbors)


-- Private API.
-- ---------------------------------------------------------------------------

-- For every cell in a World, add all adjacent cells to the World.
withAdjacent :: World -> World
withAdjacent = foldr f Set.empty
    where f cell world = Set.insert cell world `union` adjacentCells cell

-- Return the set of all adjacent cells to a given cell.
adjacentCells :: Cell -> Set.Set Cell
adjacentCells cell = Set.map (vPlus cell) dirs
    where (x1, y1) `vPlus` (x2, y2) = (x1 + x2, y1 + y2)
          dirs = Set.fromList [(x, y) | x <- ds, y <- ds, x /= 0 || y /= 0]
          ds = [(-1) .. 1]

-- Return a list of pairs (i, x) where `i` is the index of item `x` in the
-- given list.
iterIndices :: (Enum n, Num n) => [a] -> [(n, a)]
iterIndices xs = zip [0..] xs
