module Days.Day1 where

day1 :: IO ()
day1 = do
  input <- readFile "input/day1.in"
  let numbers = map (read . return) input
  let answer = sum $ fst <$> filter (uncurry (==)) (zipAdj numbers)
  print answer

zipAdj :: [a] -> [(a, a)]
zipAdj (x:xs) = zip (x:xs) (xs ++ [x])
zipAdj []     = []
