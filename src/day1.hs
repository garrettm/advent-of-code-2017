module Day1 (part1, part2) where

import Data.Char

part1 :: IO Int
part1 = do
  contents <- getLine
  let digits = map digitToInt contents
  let both = zip digits $ drop 1 (cycle digits)
  pure $ sumIfEqual both

part2 :: IO Int
part2 = do
  contents <- getLine
  let digits = map digitToInt contents
  let both = zip digits $ drop (length digits `div` 2) (cycle digits)
  pure $ sumIfEqual both

sumIfEqual :: [(Int, Int)] -> Int
sumIfEqual [] = 0
sumIfEqual ((a, b):xs) = this + sumIfEqual xs
  where
    this = if a == b then a else 0
