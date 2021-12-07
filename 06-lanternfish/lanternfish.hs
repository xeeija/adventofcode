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
solve' n = sum . (!! n) . iterate simulate' . count

simulate' :: (Num a) => [a] -> [a]
simulate' [a, b, c, d, e, f, g, h, i] = [b, c, d, e, f, g, h + a, i, a]

-- count :: (Integral a) => [a] -> [Integer]
count :: [Integer] -> [Integer]
count = map (toInteger . subtract 1 . length) . group . sort . (++ [0 .. 8])

-- Test cases

inputTest = [3, 4, 3, 1, 2]

-- >>> solve' 80 inputTest
-- 5934

-- >>> count input
-- [0,124,43,33,55,45,0,0,0]

-- >>> simulate' (count input)
-- >>> sum $ iterate simulate' (count inputTest) !! 256
-- [124,43,33,55,45,0,0,0,0]
-- 26984457539
