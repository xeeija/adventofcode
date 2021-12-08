module Main where

import Data.List
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  f <- readFile (head (filter (\(x : xs) -> x /= '-') args))

  print $ choose args (lines f)

choose :: [String] -> [String] -> Int
choose args
  | "-2" `elem` args = \[] -> 1
  | otherwise = \[] -> 1

readBoard :: String -> [[Int]]
readBoard = map (map read . words) . lines

