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

