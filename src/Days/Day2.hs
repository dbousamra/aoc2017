module Days.Day2 where

import           Data.Foldable
import           Data.List
import           Data.List.Split
import           Data.Maybe

day2 :: IO ()
day2 = do
  input <- readFile "input/day2.in"
  let rows = inputToRows input
  let differences = difference <$> rows
  let answer = sum differences
  let answerExtended = sum $ concat $ do
        n <- rows
        i <- n
        pure $ isDivisibleEvenly i n
  print answer
  print answerExtended

inputToRows :: String -> [[Int]]
inputToRows input = (fmap . fmap) read (splitOn "\t") <$> lines input

difference :: [Int] -> Int
difference row = maximum row - minimum row


isDivisibleEvenly :: Int -> [Int] -> [Int]
isDivisibleEvenly n ns = mapMaybe f nsf
  where
    f x = if n `mod` x == 0 then Just (n `div` x) else Nothing
    nsf = filter (/= n) ns
