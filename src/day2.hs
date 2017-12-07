module Day2 (part1, part2) where

import Data.Char
import Data.List
import Control.Monad

parse :: String -> [[Int]]
parse str = map ((map read) . words) $ lines str

part1 :: String -> Int
part1 i = sum $ map rowMinMax (parse i)
  where
    rowMinMax nums = maximum nums - minimum nums

part2 :: String -> Int
part2 i = sum $ parse i >>= rowDivisors

rowDivisors :: [Int] -> [Int]
rowDivisors nums = do
  i <- nums
  j <- nums
  guard (i `mod` j == 0)
  let d = i `div` j
  guard (d /= 1)
  pure d





  -- [d | i <- nums,
  --                       j <- nums,
  --                       i `mod` j == 0
  --                       let d = i `div` j,
  --                       d /= 1]
