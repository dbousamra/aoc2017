module Main where

import           System.Environment

main :: IO ()
main = do
  day <- head <$> getArgs
  putStrLn day
