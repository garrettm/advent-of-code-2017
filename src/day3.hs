module Day3 (part1, part2) where

import Data.List
import Data.Maybe
import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad.ST

data Pt = Pt Int Int deriving (Eq, Show, Ord)
type Board = Map Pt Int
data Dir = N | S | E | W deriving (Eq, Show, Ord)

step :: Dir -> Pt -> Pt
step N (Pt x y) = Pt x (y+1)
step S (Pt x y) = Pt x (y-1)
step E (Pt x y) = Pt (x+1) y
step W (Pt x y) = Pt (x-1) y

turnLeft :: Dir -> Dir
turnLeft N = W
turnLeft W = S
turnLeft S = E
turnLeft E = N

advance :: Pt -> Dir -> Board -> (Pt, Dir)
advance pt dir board = if M.member leftPt board then (step dir pt, dir) else (leftPt, leftDir)
  where
    leftDir = turnLeft dir
    leftPt = step leftDir pt

grid :: Pt -> Int -> Dir -> Board -> [(Pt, Int, Dir, Board)]
grid pt pos dir prev = (nextPt, nextPos, nextDir, nextBoard) : (grid nextPt nextPos nextDir nextBoard)
  where
    (nextPt, nextDir) = advance pt dir prev
    nextPos = pos + 1
    nextBoard = M.insert nextPt nextPos prev

part1 i = ((abs x) + (abs y), Pt x y, pos, dir)
  where
    something = grid (Pt 0 0) 1 S (M.fromList [(Pt 0 0, 1)])
    target = (read i) - 2   -- offset by 2 because of weird grid initial conditions which I am not fixing right now
    ((Pt x y), pos, dir, _) = something !! target

neighbors :: Pt -> [Pt]
neighbors (Pt x y) = [
  Pt (x+1) y,
  Pt (x+1) (y+1),
  Pt x (y+1),
  Pt (x-1) (y+1),
  Pt (x-1) y,
  Pt (x-1) (y-1),
  Pt x (y-1),
  Pt (x+1) (y-1)
  ]

grid2 :: Pt -> Dir -> Board -> [(Pt, Int, Dir, Board)]
grid2 pt dir prev = (nextPt, nextVal, nextDir, nextBoard) : (grid2 nextPt nextDir nextBoard)
  where
    (nextPt, nextDir) = advance pt dir prev
    neighborsMaybe = map ((flip M.lookup) prev) (neighbors nextPt)
    nextVal = (sum . catMaybes) neighborsMaybe
    nextBoard = M.insert nextPt nextVal prev

part2 i = (val, pt, dir, board)
  where
    input = (read i) :: Int
    grids = grid2 (Pt 0 0) S (M.fromList [(Pt 0 0, 1)])
    [(pt, val, dir, board)] = take 1 $ filter (\(_, val, _, _) -> input < val) grids
