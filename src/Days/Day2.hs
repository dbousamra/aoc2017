module Days.Day2 where

import           Data.List.Split

day2 :: IO ()
day2 = do
  input <- readFile "input/day2.in"
  let rows = inputToRows input
  let differences = difference <$> rows
  let answer = sum differences
  print answer

inputToRows :: String -> [[Int]]
inputToRows input = (fmap . fmap) read (splitOn "\t") <$> lines input

difference :: [Int] -> Int
difference row = maximum row - minimum row
