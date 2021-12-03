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

