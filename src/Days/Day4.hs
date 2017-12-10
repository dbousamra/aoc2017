module Days.Day4 where

import           Data.List

day4 :: IO ()
day4 = do
  input <- readFile "input/day4.in"
  let phrases = passphrases input
  print $ part1 phrases
  print $ part2 phrases

type Passphrase = [String]

part1 :: [Passphrase] -> Int
part1 = length . filter notDuplicate

part2 :: [Passphrase] -> Int
part2 = length . filter notAnagram

notDuplicate :: Passphrase -> Bool
notDuplicate phrase = phrase == nub phrase

notAnagram :: Passphrase -> Bool
notAnagram = notDuplicate .  map sort

passphrases :: String -> [Passphrase]
passphrases input = fmap words (lines input)
