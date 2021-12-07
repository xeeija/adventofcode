module Main where

import Data.List
import Data.Time.Clock (getCurrentTime)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  f <- readFile $ head $ filter (\(x : xs) -> x /= '-') args

  -- print $ choose args (map read $ words f)

  mapM_ printWithDate $ zip [5, 10 ..] $ map (choose args (map read $ words f)) [5, 10 .. (read $ args !! 2)]

printWithDate :: Show a => a -> IO ()
printWithDate x = do
  print x
  t <- getCurrentTime
  print t

-- import Control.Monad ((<=<))
-- (print <=< withDate) ==

withDate :: (a, b) -> IO (a, b, String)
withDate (x, y) = do
  t <- getCurrentTime
  return (x, y, show t)

choose :: [String] -> [Int] -> Int -> Int
choose args
  | "-2" `elem` args = flip solve -- (read $ args !! 2) -- 256 is too much, because exponential growth
  | otherwise = flip solve -- 80

solve :: Int -> [Int] -> Int
solve n = length . (!! n) . simulate

simulate :: [Int] -> [[Int]]
simulate = iterate (tick =<<)

tick :: Int -> [Int]
tick 0 = [6, 8]
tick n = [n - 1]

-- Test cases

-- >>> solve 150 [3,4,3,1,2]
-- 772217

-- >>> [5,10..120]

x = iterate (concatMap tick) [3, 4, 3, 1, 2]

y x = (>>=) x tick

-- >>> (take 50 . map length) x
-- >>> y [3,4,3,1,2]
-- [6,6,12,12,12,12,12,12,12,18,18,24,24,24,24,24,30,30,42,42,48,48,48,54,54,72,72,90,90,96,102,102,126,126,162,162,186,192,198,228,228,288,288,348,354,384,420,426,516,516]
-- [2,3,2,0,1]
-- [6,6,12,12,12,12,12,12,12,18,18,24,24,24,24,24,30,30,42,42,48,48,48,54,54,72,72,90,90,96,102,102,126,126,162,162,186,192,198,228,228,288,288,348,354,384,420,426,516,516]

-- >>> solve 190 [1]
-- 19858790

-- >>> solve 80 input
-- 362639

-- >>> solve 80 [3,4,3,1,2]
-- >>> solve 80 [3,2]
-- >>> solve 80 [3,1,4]
-- 5934
-- 2345
-- 3589

--

input :: [Int]
input = [2, 1, 2, 1, 5, 1, 5, 1, 2, 2, 1, 1, 5, 1, 4, 4, 4, 3, 1, 2, 2, 3, 4, 1, 1, 5, 1, 1, 4, 2, 5, 5, 5, 1, 1, 4, 5, 4, 1, 1, 4, 2, 1, 4, 1, 2, 2, 5, 1, 1, 5, 1, 1, 3, 4, 4, 1, 2, 3, 1, 5, 5, 4, 1, 4, 1, 2, 1, 5, 1, 1, 1, 3, 4, 1, 1, 5, 1, 5, 1, 1, 5, 1, 1, 4, 3, 2, 4, 1, 4, 1, 5, 3, 3, 1, 5, 1, 3, 1, 1, 4, 1, 4, 5, 2, 3, 1, 1, 1, 1, 3, 1, 2, 1, 5, 1, 1, 5, 1, 1, 1, 1, 4, 1, 4, 3, 1, 5, 1, 1, 5, 4, 4, 2, 1, 4, 5, 1, 1, 3, 3, 1, 1, 4, 2, 5, 5, 2, 4, 1, 4, 5, 4, 5, 3, 1, 4, 1, 5, 2, 4, 5, 3, 1, 3, 2, 4, 5, 4, 4, 1, 5, 1, 5, 1, 2, 2, 1, 4, 1, 1, 4, 2, 2, 2, 4, 1, 1, 5, 3, 1, 1, 5, 4, 4, 1, 5, 1, 3, 1, 3, 2, 2, 1, 1, 4, 1, 4, 1, 2, 2, 1, 1, 3, 5, 1, 2, 1, 3, 1, 4, 5, 1, 3, 4, 1, 1, 1, 1, 4, 3, 3, 4, 5, 1, 1, 1, 1, 1, 2, 4, 5, 3, 4, 2, 1, 1, 1, 3, 3, 1, 4, 1, 1, 4, 2, 1, 5, 1, 1, 2, 3, 4, 2, 5, 1, 1, 1, 5, 1, 1, 4, 1, 2, 4, 1, 1, 2, 4, 3, 4, 2, 3, 1, 1, 2, 1, 5, 4, 2, 3, 5, 1, 2, 3, 1, 2, 2, 1, 4]

inputCount = ((\x -> (map head x, map length x)) . group . sort) input

-- >>> input
-- ([1,2,3,4,5],[124,43,33,55,45])

-- 124 -> 255
--  43 -> 254
--  33 -> 253
--  55 -> 252
--  45 -> 251
