module Main where

import Data.List
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  f <- readFile (head args)

  putStr "lad: "
  print $ lad $ read $ "[" ++ f ++ "]"

  putStr "least squares: "
  print $ minLs $ read $ "[" ++ f ++ "]"

lad :: Integral a => [a] -> a
lad xs = sum $ map (abs . (median xs -)) xs

median :: Integral a => [a] -> a
median xs
  | odd $ length xs = s mid
  | otherwise = (s mid + s (mid - 1)) `div` 2
  where
    mid = length xs `div` 2
    s = (sort xs !!)

-- Part two

-- Because mean is a float usually,
-- calculate leastSquares with floor mean and ceil mean and take the min

minLs :: (Integral a) => [a] -> a
minLs x = minimum [leastSquares (floor $ mean x) x, leastSquares (ceiling $ mean x) x]

leastSquares :: Integral a => a -> [a] -> a
leastSquares m xs = sum $ map (sumTo . abs . (m -)) xs

-- leastSquares xs = sum $ map (sumTo . abs . (mean xs -)) xs

sumTo :: Integral a => a -> a
sumTo n = (n + n ^ 2) `div` 2

-- mean :: (Integral a, Integral b) => [a] -> b
-- mean xs = round $ fromIntegral (sum xs) / fromIntegral (length xs)

mean :: (Integral a, Fractional b) => [a] -> b
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- Test cases

testInput :: [Int]
testInput = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

-- >>> median testInput
-- >>> lad testInput
-- 2
-- 37

-- >>> mean testInput
-- >>> leastSquares testInput
-- 5
-- 168

-- >>> mean input
-- >>> leastSquares input
-- 471
-- 95476244

-- >>> mean input
-- >>> leastSquares input
-- 472
-- 95476248

-- >>> map (flip leastSquares input) [470..474]
-- [95477241,95476244,95476248,95477253,95479258]

-- >>> map (abs . (5 -)) testInput
-- >>> map (sumTo . abs . (5 -)) testInput
-- [11,4,3,5,1,3,2,4,3,9]
-- [66,10,6,15,1,6,3,10,6,45]

-- >>> sort testInput
-- >>> length testInput
-- [0,1,1,2,2,2,4,7,14,16]
-- 10
