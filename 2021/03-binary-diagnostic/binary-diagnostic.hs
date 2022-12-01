module Main where

import Data.Bits (Bits (complement, popCount))
import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (foldl', transpose)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  f <- readFile (head (filter (\(x : xs) -> x /= '-') args))

  print $ choose args (lines f)

choose :: [String] -> [String] -> Int
choose args
  | "-2" `elem` args = lifeSupport
  | otherwise = consumption

consumption :: [String] -> Int
consumption xs = gamma (transpose xs) * epsilon (transpose xs)

gamma :: [String] -> Int
gamma = toDec . map (commonBit (>))

epsilon :: [String] -> Int
epsilon = toDec . map (commonBit (<=))

commonBit :: (Num a) => (a -> a -> Bool) -> String -> Char
commonBit pred = maxBy pred . foldr count (0, 0)
  where
    count :: (Num a1, Num a2) => Char -> (a1, a2) -> (a1, a2)
    count '0' (a, b) = (a + 1, b)
    count '1' (a, b) = (a, b + 1)
    count _ x = x

maxBy :: (t1 -> t2 -> Bool) -> (t1, t2) -> Char
maxBy pred (a, b)
  | pred a b = '0'
  | otherwise = '1'

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

-- >>> (map (commonBit (<)) . transpose) ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]
-- "01001"

-- >>> consumption ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]
-- 198

-- bits' = map (maxBy (>) . complement popCount . toDec) . transpose

-- >>> (map ((\x -> (popCount (complement x), popCount x)) . toDec) . transpose) ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]
-- [(57,7),(59,5),(56,8),(57,7),(59,5)]

lifeSupport xs = oxygen xs * co2scrubber xs

oxygen :: [String] -> Int
oxygen = toDec . filterBitCriteria (>) 0

co2scrubber :: [String] -> Int
co2scrubber = toDec . filterBitCriteria (<=) 0

filterBitCriteria :: (Num a) => (a -> a -> Bool) -> Int -> [String] -> String
-- case [] not covered
filterBitCriteria f _ [x] = x
filterBitCriteria f n xs = filterBitCriteria f (n + 1) $ filter (compareNth criteria) xs
  where
    criteria = map (commonBit f) (transpose xs)
    compareNth = (==) `on` (!! n)

-- compareNth :: (Eq a) => [a] -> [a] -> Bool

-- diag = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]

-- >>> oxygen diag
-- >>> co2scrubber diag
-- >>> lifeSupport diag
-- 23
-- 10
-- 230

-- >>> diag
-- >>> transpose diag
-- >>> (map (commonBit (>)) . transpose) diag
-- ["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]
-- ["011110011100","010001010101","111111110000","011101100011","000111100100"]
-- "10110"

-- >>> filter (compareNth 0 "1 0110") diag
-- >>> filter (compareNth 1 "10 100") ["11110","10110","10111","10101","11100","10000","11001"]
-- >>> filter (compareNth 2 "101 11") ["10110","10111","10101","10000"]
-- >>> filter (compareNth 3 "1011 1") ["10110","10111","10101"]
-- >>> filter (compareNth 4 "10111 ") ["10110","10111"]
-- ["11110","10110","10111","10101","11100","10000","11001"]
-- ["10110","10111","10101","10000"]
-- ["10110","10111","10101"]
-- ["10110","10111"]
-- ["10111"]

-- Most common bits for above
-- >>> (map (commonBit (>)) . transpose) ["11110","10110","10111","10101","11100","10000","11001"]
-- >>> (map (commonBit (>)) . transpose) ["10110","10111","10101","10000"]
-- >>> (map (commonBit (>)) . transpose) ["10110","10111","10101"]
-- >>> (map (commonBit (>)) . transpose) ["10110","10111"]
-- "10100"
-- "10111"
-- "10111"
-- "10111"
