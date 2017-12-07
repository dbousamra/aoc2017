module Days.Day1 where

day1 :: IO ()
day1 = do
  input <- readFile "input/day1.in"
  let numbers = map (read . return) input
  let answer = checksum 1 numbers
  let answerExtended = checksum (length numbers `div` 2) numbers
  print answer
  print answerExtended

checksum :: Int -> [Int] -> Int
checksum offset numbers = sum $ fst <$> filter (uncurry (==)) zippedNumbers
  where zippedNumbers = zipAdjOffset offset numbers

zipAdjOffset :: Int -> [a] -> [(a, a)]
zipAdjOffset offset xs = zip xs (drop offset (xs ++ xs))
