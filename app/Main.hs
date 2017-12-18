module Main where

import Data.List
import Data.Char
import Day3

main :: IO ()
main = do
  raw <- (dropWhileEnd isSpace) <$> readFile "data/day3.txt"
  putStrLn $ show $ part1 raw
  putStrLn $ show $ part2 raw
