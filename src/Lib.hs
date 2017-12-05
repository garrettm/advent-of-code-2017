module Lib
    ( someFunc
    ) where

import Day1

someFunc :: IO ()
someFunc = do
  result <- part2
  putStrLn $ show result
