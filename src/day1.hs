module Day1 (part1, part2) where

import Data.Char

part1 :: String -> Int
part1 contents = sumIfEqual both
  where
    digits = map digitToInt contents
    both = zip digits $ drop 1 (cycle digits)

part2 :: String -> Int
part2 contents = sumIfEqual both
  where
    digits = map digitToInt contents
    both = zip digits $ drop (length digits `div` 2) (cycle digits)

sumIfEqual :: [(Int, Int)] -> Int
sumIfEqual [] = 0
sumIfEqual ((a, b):xs) = this + sumIfEqual xs
  where
    this = if a == b then a else 0
