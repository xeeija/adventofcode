module Main where

import Data.List
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  f <- readFile (head args)

  print $ solve' (read $ args !! 1) (map read $ words f)

-- Initial, naive solution - exponential time (and space?) complexity O(2^n)

solve :: Int -> [Int] -> Int
solve n = length . (!! n) . simulate

simulate :: [Int] -> [[Int]]
simulate = iterate (tick =<<)

tick :: Int -> [Int]
tick 0 = [6, 8]
tick n = [n - 1]

-- Improved solution, linear time and space complexity

-- solve' :: Num a => Int -> [a] -> a
solve' :: Int -> [Integer] -> Integer
solve' n = sum . (!! n) . iterate simulate' . hist

simulate' :: (Num a) => [a] -> [a]
simulate' [a, b, c, d, e, f, g, h, i] = [b, c, d, e, f, g, h + a, i, a]

-- count :: (Integral a) => [a] -> [Integer]
count :: [Integer] -> [Integer]
count = map (toInteger . subtract 1 . length) . group . sort . (++ [0 .. 8])

-- Tipp: subtract 1 == pred

hist :: (Eq a, Num a, Enum a) => [a] -> [Integer]
hist xs = map (`count'` xs) [0 .. 8]

count' :: Eq a => a -> [a] -> Integer
count' n = toInteger . length . filter (== n)

